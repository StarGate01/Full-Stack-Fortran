#!/bin/bash

OUT=`echo "$@" | sed -n 's/.* -o \(.*\).*/\1/p' | cut -d " " -f1`
gfortran-4.6 -emit-llvm -S -flto -m32 -fverbose-asm -nostdlib -fplugin=/app/bin/dragonegg.so "$@" && \
sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-f128:64-S128"/' "$OUT" && \
sed -i 's/^\(target\striple\s*=\s*\).*$/\1"wasm32-unknown-emscripten"/' "$OUT" && \
llvm-as-3.3 "$OUT" -o "$OUT.bc" && \
mv "$OUT" "$OUT.ll" && \
mv "$OUT.bc" "$OUT" && \
if [ -z "$KEEP_TEMPS" ]; then rm "$OUT.ll"; fi