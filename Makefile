.PHONY: all clean lint test check cli interop interop-go-server conformance

all: cli

cli: ag-proto/*.lisp ag-proto-cli/*.lisp *.asd 
	sbcl --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :directory (merge-pathnames \"ag-proto-cli/\" (uiop:getcwd))) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd)))))" \
	     --eval "(asdf:make :ag-proto-cli)" --quit
	mv ag-proto-cli/ag-protoc .

clean:
	rm -rf *~ ag-protoc

lint:
	ocicl lint ag-proto.asd ag-http2.asd ag-grpc.asd ag-grpc-all.asd ag-proto-cli/ag-proto-cli.asd

test check:
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :ag-grpc-all/tests :force t)" \
	     --eval "(let ((results (fiveam:run 'ag-grpc-tests:ag-grpc-all-tests))) (format t \"~&~%Test Results:~%\") (fiveam:explain! results) (finish-output) (if (fiveam:results-status results) (sb-ext:exit :code 0) (sb-ext:exit :code 1)))"

interop-go-server:
	cd interop/go-server && go build -buildvcs=false -o hello-server .

interop: interop-go-server
	@set -euo pipefail; \
	if pgrep -x "hello-server" >/dev/null 2>&1; then \
	  pkill -x "hello-server"; \
	  sleep 0.2; \
	fi; \
	export XDG_CACHE_HOME="$(CURDIR)/.cache"; \
	rm -rf "$$XDG_CACHE_HOME/common-lisp"; \
	mkdir -p "$$XDG_CACHE_HOME"; \
	GODEBUG=http2debug=2 GRPC_GO_LOG_SEVERITY_LEVEL=info GRPC_GO_LOG_VERBOSITY_LEVEL=99 GRPC_TRACE=http,transport,api ./interop/go-server/hello-server & server_pid=$$!; \
	trap 'kill $$server_pid >/dev/null 2>&1 || true' EXIT; \
	sleep 0.2; \
	sbcl --script interop/test-client.lisp

conformance: interop/interop-server
	@echo "Running gRPC conformance tests..."
	@cd interop && ./interop-server & server_pid=$$!; \
	trap 'kill $$server_pid >/dev/null 2>&1 || true' EXIT; \
	sleep 0.5; \
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (truename \"$(CURDIR)\")) (list :tree (merge-pathnames \"ocicl/\" (truename \"$(CURDIR)\")))))" \
	     --eval "(asdf:load-system :ag-grpc)" \
	     --load interop/interop.lisp \
	     --load interop/run-tests.lisp

interop/interop-server: interop/server.go interop/pb/*.go
	cd interop && go build -o interop-server server.go
