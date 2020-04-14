FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends cmake python3 make bash git ca-certificates \
        xz-utils flang-7 clang-8 libflang0d-7 llvm-8 llvm-8-tools llvm-8-runtime lld-7  && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
RUN cd emsdk && \
    ./emsdk install 1.39.12 && \
    ./emsdk activate 1.39.12

RUN apt-get update && \
    apt-get -y install --no-install-recommends python3-distutils  && \
    rm -rf /var/lib/apt/lists/*

    