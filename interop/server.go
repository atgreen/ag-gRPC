// gRPC interop test server for ag-gRPC conformance testing
package main

import (
	"bytes"
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"

	pb "interop-server/pb"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"
)

var (
	port = flag.Int("port", 10000, "The server port")
)

type testServer struct {
	pb.UnimplementedTestServiceServer
}

// EmptyCall returns an empty response
func (s *testServer) EmptyCall(ctx context.Context, in *pb.Empty) (*pb.Empty, error) {
	return &pb.Empty{}, nil
}

// UnaryCall returns a response with the requested payload size
func (s *testServer) UnaryCall(ctx context.Context, in *pb.SimpleRequest) (*pb.SimpleResponse, error) {
	log.Printf("UnaryCall received: ResponseType=%v, ResponseSize=%d, ResponseStatus=%+v",
		in.ResponseType, in.ResponseSize, in.ResponseStatus)

	// Echo back custom metadata
	if md, ok := metadata.FromIncomingContext(ctx); ok {
		// Echo initial metadata
		if vals := md.Get("x-grpc-test-echo-initial"); len(vals) > 0 {
			header := metadata.Pairs("x-grpc-test-echo-initial", vals[0])
			grpc.SendHeader(ctx, header)
		}
		// Echo trailing metadata
		if vals := md.Get("x-grpc-test-echo-trailing-bin"); len(vals) > 0 {
			trailer := metadata.Pairs("x-grpc-test-echo-trailing-bin", vals[0])
			grpc.SetTrailer(ctx, trailer)
		}
	}

	// Check if we should return an error status
	if in.ResponseStatus != nil {
		return nil, status.Error(codes.Code(in.ResponseStatus.Code), in.ResponseStatus.Message)
	}

	// Create response payload
	payload := &pb.Payload{
		Type: in.ResponseType,
		Body: make([]byte, in.ResponseSize),
	}

	return &pb.SimpleResponse{
		Payload:  payload,
		ServerId: "ag-grpc-test-server",
		Hostname: getHostname(),
	}, nil
}

// StreamingOutputCall returns multiple responses
func (s *testServer) StreamingOutputCall(in *pb.StreamingOutputCallRequest, stream pb.TestService_StreamingOutputCallServer) error {
	// Check if we should return an error status
	if in.ResponseStatus != nil && in.ResponseStatus.Code != 0 {
		return status.Error(codes.Code(in.ResponseStatus.Code), in.ResponseStatus.Message)
	}

	for _, param := range in.ResponseParameters {
		payload := &pb.Payload{
			Type: in.ResponseType,
			Body: make([]byte, param.Size),
		}
		if err := stream.Send(&pb.StreamingOutputCallResponse{Payload: payload}); err != nil {
			return err
		}
	}
	return nil
}

// StreamingInputCall aggregates payloads from client
func (s *testServer) StreamingInputCall(stream pb.TestService_StreamingInputCallServer) error {
	var totalSize int32
	for {
		in, err := stream.Recv()
		if err == io.EOF {
			return stream.SendAndClose(&pb.StreamingInputCallResponse{
				AggregatedPayloadSize: totalSize,
			})
		}
		if err != nil {
			return err
		}
		if in.Payload != nil {
			totalSize += int32(len(in.Payload.Body))
		}
	}
}

// FullDuplexCall echoes back each request
func (s *testServer) FullDuplexCall(stream pb.TestService_FullDuplexCallServer) error {
	// Echo metadata
	if md, ok := metadata.FromIncomingContext(stream.Context()); ok {
		if vals := md.Get("x-grpc-test-echo-initial"); len(vals) > 0 {
			header := metadata.Pairs("x-grpc-test-echo-initial", vals[0])
			stream.SendHeader(header)
		}
		if vals := md.Get("x-grpc-test-echo-trailing-bin"); len(vals) > 0 {
			trailer := metadata.Pairs("x-grpc-test-echo-trailing-bin", vals[0])
			stream.SetTrailer(trailer)
		}
	}

	for {
		in, err := stream.Recv()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		// Check for error status request
		if in.ResponseStatus != nil && in.ResponseStatus.Code != 0 {
			return status.Error(codes.Code(in.ResponseStatus.Code), in.ResponseStatus.Message)
		}

		// Send responses for each parameter
		for _, param := range in.ResponseParameters {
			payload := &pb.Payload{
				Type: in.ResponseType,
				Body: make([]byte, param.Size),
			}
			if err := stream.Send(&pb.StreamingOutputCallResponse{Payload: payload}); err != nil {
				return err
			}
		}
	}
}

// HalfDuplexCall buffers requests then sends responses
func (s *testServer) HalfDuplexCall(stream pb.TestService_HalfDuplexCallServer) error {
	var requests []*pb.StreamingOutputCallRequest
	for {
		in, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		requests = append(requests, in)
	}

	for _, req := range requests {
		for _, param := range req.ResponseParameters {
			payload := &pb.Payload{
				Type: req.ResponseType,
				Body: make([]byte, param.Size),
			}
			if err := stream.Send(&pb.StreamingOutputCallResponse{Payload: payload}); err != nil {
				return err
			}
		}
	}
	return nil
}

// UnimplementedCall is intentionally not implemented
func (s *testServer) UnimplementedCall(ctx context.Context, in *pb.Empty) (*pb.Empty, error) {
	return nil, status.Error(codes.Unimplemented, "intentionally unimplemented")
}

func getHostname() string {
	hostname, err := os.Hostname()
	if err != nil {
		return "unknown"
	}
	return hostname
}

func main() {
	flag.Parse()

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	pb.RegisterTestServiceServer(s, &testServer{})

	log.Printf("gRPC interop test server listening on port %d", *port)

	// Write a marker file to indicate server is ready
	readyFile := fmt.Sprintf("/tmp/grpc-interop-server-%d.ready", *port)
	os.WriteFile(readyFile, []byte("ready"), 0644)
	defer os.Remove(readyFile)

	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}

// Unused but required for compilation
var _ = bytes.Buffer{}
