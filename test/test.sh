#!/bin/bash

cd "${0%/*}/../tools"
DNAME=`readlink -f ../test`
docker-compose run --rm -v $DNAME:/project wasmchain bash -c 'cd /project && VERBOSE=1 make build'
docker kill testserver
docker rm testserver
docker run --name testserver -p 8080:3000 -v $DNAME/bin:/app/public:ro -d tobilg/mini-webserver