FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash flang-7 libflang0d-7 make cmake \
        g++ build-essential python3 python3-distutils git gnutls-bin ca-certificates xz-utils wget cproto python3-pip xz-utils && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

COPY scripts /app/scripts

RUN git clone https://github.com/StarGate01/flang.git

RUN apt-get update && \
    apt-get -y install --no-install-recommends llvm-7-dev cmake-curses-gui && \
    rm -rf /var/lib/apt/lists/*

COPY flang flang
RUN cd flang && \
    mkdir build && \
    cd build && \
    CFLAGS=-I/app/emsdk/upstream/emscripten/system/lib/libc/musl/src/internal \
        emcmake cmake -DLLVM_CONFIG=llvm-config-7 -DTARGET_ARCHITECTURE=wasm32 .. && \
    make