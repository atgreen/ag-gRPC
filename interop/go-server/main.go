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
