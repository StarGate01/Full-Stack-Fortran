#!/bin/bash

cd "${0%/*}/../tools"
docker-compose run --rm -v ../test:/project wasmchain bash -c 'cd /project && make -f makefile.debug build'
docker run --name testserver -p 8080:3000 -v ../test/bin:/app/public:ro -d tobilg/mini-webserver