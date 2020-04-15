#!/bin/bash
llvm-config "$@" | python -c 'import sys;print sys.stdin.read().replace("-std=c++14","")' #hehehe