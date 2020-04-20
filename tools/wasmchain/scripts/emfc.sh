#!/bin/bash

OUT=`echo "$@" | sed -n 's/.* -o \(.*\).*/\1/p' | cut -d " " -f1`
gfortran-4.6 -emit-llvm -S -flto -m32 -fverbose-asm -nostdlib -fplugin=/app/dragonegg-3.3.src/dragonegg.so "$@" && \
sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-m:e-p:32:32-i64:64-n32:64-S128"/' "$OUT" && \
sed -i 's/^\(target\striple\s*=\s*\).*$/\1"wasm32-unknown-emscripten"/' "$OUT" && \
llvm-as-3.3 "$OUT" -o "$OUT.bc" && \
mv "$OUT" "$OUT.ll" && \
mv "$OUT.bc" "$OUT" && \
if [ -z "$KEEP_TEMPS" ]; then rm "$OUT.ll"; fi

# #!/bin/bash -x

# OUT=`echo "$@" | sed -n 's/.* -o \(.*\).*/\1/p' | cut -d " " -f1`
# gfortran-4.6 -emit-llvm -S -flto -m32 -fverbose-asm -nostdlib -fplugin=/app/dragonegg-3.3.src/dragonegg.so "$@"
# if [ -z "$OUT" ]; then
#     LAST=`echo "$@" | rev | cut -d' ' -f1 | rev`
#     LAST=`echo "$LAST" | rev | cut -d"." -f2- | rev`
#     EXT="out"
#     if [[ $@ == *"-c"* ]]; then
#         EXT="o"
#     elif [[ $@ == *"-S"* ]]; then
#         EXT="s"
#     fi
#     OUT="$LAST.$EXT"
#     mv "$LAST.s" "$LAST.$EXT"
#     echo "OUT1: $OUT"
# fi
# echo "OUT2: $OUT"
# sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-m:e-p:32:32-i64:64-n32:64-S128"/' "$OUT"
# sed -i 's/^\(target\striple\s*=\s*\).*$/\1"wasm32-unknown-emscripten"/' "$OUT"
# llvm-as-3.3 "$OUT" -o "$OUT.bc"
# mv "$OUT" "$OUT.ll"
# if [ -z "$KEEP_TEMPS" ]; then rm "$OUT.ll"; fi
# if [[ $@ != *"-c"* && $@ != *"-S"*  && $@ != *"-E"* ]]; then
#     echo "Compiling"
#     emcc -m32 -c -o "$OUT" "$OUT.bc"
# else
#     echo "Not Compiling"
#     mv "$OUT.bc" "$OUT"
# fi