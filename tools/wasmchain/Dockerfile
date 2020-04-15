FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash flang-7 libflang0d-7 llvm-7 llvm-7-tools llvm-7-runtime lld-7 \
        build-essential python3 python3-distutils git gnutls-bin ca-certificates xz-utils && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

RUN apt-get update && \
    apt-get -y install --no-install-recommends cproto && \
    rm -rf /var/lib/apt/lists/*

COPY scripts /app/scripts