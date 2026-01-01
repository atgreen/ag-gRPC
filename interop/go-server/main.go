package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"time"

	"google.golang.org/grpc"
	pb "hello-server/hello"
)

var port = flag.Int("port", 50051, "The server port")

type server struct {
	pb.UnimplementedGreeterServer
}

// Unary RPC
func (s *server) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloReply, error) {
	log.Printf("Received: %v", in.GetName())
	return &pb.HelloReply{Message: "Hello " + in.GetName()}, nil
}

// Server streaming RPC
func (s *server) SayHelloStream(in *pb.HelloRequest, stream pb.Greeter_SayHelloStreamServer) error {
	name := in.GetName()
	count := in.GetCount()
	if count <= 0 {
		count = 5 // Default to 5 messages
	}
	log.Printf("SayHelloStream: name=%v, count=%v", name, count)

	for i := int32(0); i < count; i++ {
		reply := &pb.HelloReply{
			Message: fmt.Sprintf("Hello %s (#%d)", name, i+1),
			Index:   i,
		}
		if err := stream.Send(reply); err != nil {
			return err
		}
		log.Printf("  Sent message %d/%d", i+1, count)
		// Small delay to make streaming visible
		time.Sleep(100 * time.Millisecond)
	}
	log.Printf("SayHelloStream complete")
	return nil
}

// Client streaming RPC
func (s *server) CollectHellos(stream pb.Greeter_CollectHellosServer) error {
	var names []string
	count := int32(0)

	log.Printf("CollectHellos: starting to receive messages")
	for {
		req, err := stream.Recv()
		if err != nil {
			// End of stream
			break
		}
		count++
		names = append(names, req.GetName())
		log.Printf("  Received message %d: name=%v", count, req.GetName())
	}

	combined := ""
	for i, name := range names {
		if i > 0 {
			combined += ", "
		}
		combined += name
	}

	log.Printf("CollectHellos complete: received %d messages", count)
	return stream.SendAndClose(&pb.HelloSummary{
		TotalRequests: count,
		CombinedNames: combined,
	})
}

// Bidirectional streaming RPC - echo chat
func (s *server) Chat(stream pb.Greeter_ChatServer) error {
	log.Printf("Chat: starting bidirectional stream")
	msgNum := int32(0)
	for {
		req, err := stream.Recv()
		if err != nil {
			// End of client stream
			log.Printf("Chat: client stream ended after %d messages", msgNum)
			return nil
		}
		msgNum++
		log.Printf("Chat: received message %d: %v", msgNum, req.GetName())

		// Echo back with a greeting
		reply := &pb.HelloReply{
			Message: fmt.Sprintf("Hello %s!", req.GetName()),
			Index:   msgNum - 1,
		}
		if err := stream.Send(reply); err != nil {
			log.Printf("Chat: error sending reply: %v", err)
			return err
		}
		log.Printf("Chat: sent reply %d", msgNum)
	}
}

func main() {
	flag.Parse()
	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &server{})
	log.Printf("server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
