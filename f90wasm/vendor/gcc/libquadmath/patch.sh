#!/bin/bash

echo "Patching libquadmath"

PATCHPATH=/app/libquadmath-build
SRCPATH=/app/gcc/libquadmath

cd $SRCPATH
cp $PATCHPATH/patch/* .
