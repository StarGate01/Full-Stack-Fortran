#!/bin/bash

echo "Patching libgfortran"

PATCHPATH=/app/libgfortran-build
SRCPATH=/app/gcc/libgfortran

cd $SRCPATH
cp $PATCHPATH/patch/* .
cp config/fpu-generic.h config/fpu-target.h
grep '^#' < kinds.h > kinds.inc
grep '^#' < c99_protos.h > c99_protos.inc
