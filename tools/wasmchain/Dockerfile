FROM ubuntu:18.04 AS build
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends build-essential cmake xz-utils python3 python3-distutils git make gnutls-bin wget unzip bash ca-certificates ninja-build python3 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}

ARG LLVM_REV="b43612401077912a577fd9700c7284b6e676f3fe"
RUN wget https://github.com/llvm/llvm-project/archive/${LLVM_REV}.zip -O llvm.zip
RUN unzip llvm.zip && \
    mv llvm-project* llvm && \
    rm llvm.zip
RUN cd llvm && \
    mkdir build && \
    cd build && \
    cmake -G Ninja -DCMAKE_INSTALL_PREFIX=/usr/local -DLLVM_ENABLE_PROJECTS='mlir;flang' -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=On ../llvm && \
    ninja -j 6 && \
    ninja install
    
