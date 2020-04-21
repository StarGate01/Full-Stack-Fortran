FROM ubuntu:18.04 as build
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN echo "deb http://archive.ubuntu.com/ubuntu/ trusty main restricted universe" >> /etc/apt/sources.list && \
    echo "deb http://security.ubuntu.com/ubuntu/ trusty-security main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install --no-install-recommends \
        build-essential python wget git gnutls-bin bash make ca-certificates xz-utils \
        gfortran-4.6 g++-4.6 gcc-4.6 gcc-4.6-plugin-dev llvm-3.3-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# dragonegg
RUN git clone --single-branch --branch="release_33" --depth=1 https://github.com/llvm-mirror/dragonegg.git && \
    cd dragonegg && \
    LLVM_CONFIG=llvm-config-3.3 GCC=gcc-4.6 CC=gcc-4.6 CXX=g++-4.6 make -j$(nproc) && \
    mkdir -p /app/bin && \
    mv dragonegg.so /app/bin/

# emscripten
RUN git clone --depth=1 https://github.com/emscripten-core/emsdk.git && \
    cd emsdk && \
    ./emsdk install "1.39.12" && \
    ./emsdk activate "1.39.12"
COPY scripts /app/scripts
ENV PATH="/app/bin:/app/scripts:/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

# libgfortran
RUN git clone --single-branch --branch="releases/gcc-4.6" --depth=1 https://github.com/gcc-mirror/gcc.git
COPY vendor/gfortran gcc-build
RUN cd gcc-build && \
    ./patch.sh && \
    make -j$(nproc) build

# lapack
RUN git clone --depth=1 https://github.com/Reference-LAPACK/lapack.git
COPY vendor/LAPACK/make.inc lapack/make.inc
RUN cd lapack/SRC && \
    emmake make -j$(nproc) single double


FROM ubuntu:18.04 as tool
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN echo "deb http://archive.ubuntu.com/ubuntu/ trusty main restricted universe" >> /etc/apt/sources.list && \
    echo "deb http://security.ubuntu.com/ubuntu/ trusty-security main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install --no-install-recommends python bash make cmake gfortran-4.6 llvm-3.3 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /app/bin /app/bin/
COPY --from=build /app/emsdk /app/emsdk
COPY --from=build /root/.emscripten /root/.emscripten
COPY --from=build /app/gcc-build/bin/libgfortran.a /app/lapack/*.a /app/lib/

COPY scripts /app/scripts
ENV PATH="/app/bin:/app/scripts:/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"
ENV LIBRARY_PATH="/app/lib:${LIBRARY_PATH}"

WORKDIR /app


