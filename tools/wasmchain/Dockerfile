FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash git ca-certificates \
        flang-7 clang-8 libflang0d-7 llvm-8 llvm-8-tools llvm-8-runtime lld-8 && \
    rm -rf /var/lib/apt/lists/*

# WORKDIR /app
# RUN git clone https://github.com/WebAssembly/wasi-libc
# RUN cd wasi-libc && \
#     make WASM_CC=clang-8 WASM_AR=llvm-ar-8 WASM_NM=llvm-nm-8