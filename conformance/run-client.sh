#!/bin/bash
exec sbcl --noinform --non-interactive \
    --load /home/green/git/cl-gRPC/conformance/client.lisp
