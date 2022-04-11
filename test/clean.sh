#!/bin/bash

cd "${0%/*}"

docker run --rm -v `pwd`:/project stargate01/f90wasm bash -c 'cd /project && VERBOSE=1 make purge'