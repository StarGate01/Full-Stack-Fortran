#!/bin/bash

cp -f $1 $2
# sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-m:e-p:32:32-i64:64-n32:64-S128"/' $2
# sed -i 's/^\(target\striple\s*=\s*\).*$/\1"wasm32"/' $2

# sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-p:32:32:32-v128:32:128-n32-S128"/' $2
# sed -i 's/^\(target\striple\s*=\s*\).*$/\1"asmjs-unknown-emscripten"/' $2