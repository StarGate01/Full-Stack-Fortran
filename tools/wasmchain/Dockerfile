FROM ubuntu:18.04 AS build
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends build-essential gnutls-bin cmake xz-utils make wget bash python3 python3-distutils ca-certificates \
        git zlib1g gcc g++ libncurses-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY vendor/aocc-compiler-2.1.0_1_amd64.deb aocc-compiler-2.1.0_1_amd64.deb
RUN dpkg -i aocc-compiler-2.1.0_1_amd64.deb

RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}

COPY scripts /app/scripts
ENV PATH="/opt/AMD/aocc-compiler-2.1.0/bin:/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}" \
    LIBRARY_PATH="/opt/AMD/aocc-compiler-2.1.0/lib:/opt/AMD/aocc-compiler-2.1.0/lib32:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib32:/usr/lib:${LIBRARY_PATH}" \
    LD_LIBRARY_PATH="/opt/AMD/aocc-compiler-2.1.0/lib:/opt/AMD/aocc-compiler-2.1.0/lib32:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib32:/usr/lib:${LD_LIBRARY_PATH}" \
    C_INCLUDE_PATH="${C_INCLUDE_PATH}:/opt/AMD/aocc-compiler-2.1.0/include" \
    CPLUS_INCLUDE_PATH="${CPLUS_INCLUDE_PATH}:/opt/AMD/aocc-compiler-2.1.0/include"