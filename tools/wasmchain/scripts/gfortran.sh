#!/bin/bash


OUT=`echo "$@" | sed -n 's/.* -o \(.*\).*/\1/p' | cut -d " " -f1`
gfortran-4.6 "$@"
mv "$OUT" "$OUT.llpre"
/app/scripts/shim.sh "$OUT.llpre" "$OUT.ll"
llvm-as-3.3 "$OUT.ll" -o "$OUT.bc"
mv "$OUT.bc" "$OUT"
if [ -z "$KEEP_TEMPS" ]; then
    rm "$OUT.ll" "$OUT.llpre"
fi