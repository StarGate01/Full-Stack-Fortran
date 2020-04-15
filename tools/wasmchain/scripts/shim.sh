#!/bin/bash

cp -f $1 $2
sed -i 's/^\(target\sdatalayout\s*=\s*\).*$/\1"e-m:e-p:32:32-i64:64-n32:64-S128"/' $2
sed -i 's/^\(target\striple\s*=\s*\).*$/\1"wasm32"/' $2