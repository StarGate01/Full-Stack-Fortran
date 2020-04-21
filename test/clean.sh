#!/bin/bash

cd "${0%/*}/../tools"
DNAME=`readlink -f ../test`
docker-compose run --rm -v $DNAME:/project f90wasm bash -c 'cd /project && VERBOSE=1 make purge'