FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash flang-7 libflang0d-7 make cmake \
        g++ build-essential python3 python3-distutils git gnutls-bin ca-certificates xz-utils wget cproto python3-pip xz-utils llvm-7-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

RUN git clone https://github.com/StarGate01/flang.git
COPY lib/linux lib/linux
RUN cd lib/flang && \
    mkdir -p build && \
    cd build && \
        FC=flang-7 FFLAGS="--target=wasm32 -m32 -emit-llvm -flto -m32 -Mpreprocess -Wno-override-module" CFLAGS="--target=wasm32 -m32 -emit-llvm -flto -m32 -I/app/lib" LDFLAGS="-m32" \
        emcmake cmake -Wno-dev -DLLVM_CONFIG=llvm-config-7 -DTARGET_ARCHITECTURE=wasm32  .. && \
    make -j$(nproc)
