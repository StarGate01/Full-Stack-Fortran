#!/bin/bash

cd "${0%/*}/../tools"
docker-compose run --rm -v ../test:/project wasmchain bash -c 'cd /project && make -f makefile.test build'