#!/bin/bash

echo "Patching libgfortran"

PATCHPATH=/app/gcc-build
SRCPATH=/app/gcc/libgfortran


cd $SRCPATH
cp $PATCHPATH/patch/* .

cp config/fpu-generic.h config/fpu-target.h

cp ../libgcc/gthr-single.h ../libgcc/gthr-default.h
mv $PATCHPATH/patch/backtrace-supported.h ../libbacktrace/backtrace-supported.h

grep '^#' < kinds.h > kinds.inc
grep '^#' < c99_protos.h > c99_protos.inc