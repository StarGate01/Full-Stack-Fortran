FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN echo "deb http://archive.ubuntu.com/ubuntu/ trusty main restricted universe" >> /etc/apt/sources.list && \
    echo "deb http://security.ubuntu.com/ubuntu/ trusty-security main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install --no-install-recommends \
        build-essential python python-pip cmake \
        wget git gnutls-bin bash make ca-certificates xz-utils \
        gfortran-4.6 g++-4.6 gcc-4.6 gcc-4.6-plugin-dev llvm-3.3-dev && \
        # libfl-dev flex flexc++ \
        # gcc-multilib g++-multilib gfortran-multilib && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# # gcc
# ARG GCC_REV="66cc0b3cf5bcd0366200064192b544c9274144b3"
# RUN wget https://github.com/gcc-mirror/gcc/archive/${GCC_REV}.tar.gz -O gcc.tar.gz
# RUN tar -xzf gcc.tar.gz && \
#     mv gcc-* gcc
# RUN cd gcc && \
#     ./contrib/download_prerequisites
# COPY vendor/gcc/linux-unwind.h gcc/gcc/config/i386/linux-unwind.h
# RUN mkdir -p gcc-build gcc-bin
# RUN cd gcc-build && \
#         FC=gfortran-4.6 CXX=g++-4.6 CC=gcc-4.6 \
#         /app/gcc/configure -v \
#         --prefix=/app/gcc-bin --enable-languages=fortran,c,c++ --program-suffix=-4.6-patch \
#         --without-long-double-128 --enable-decimal-float=no --disable-multilib --disable-bootstrap
# RUN cd gcc-build && \
#     make -j$(nproc) && \
#     make install

# dragonegg
RUN wget https://llvm.org/releases/3.3/dragonegg-3.3.src.tar.gz && \
    tar xzf dragonegg-3.3.src.tar.gz && \
    cd dragonegg-3.3.src && \
        LLVM_CONFIG=llvm-config-3.3 GCC=gcc-4.6 CC=gcc-4.6 CXX=g++-4.6 \
        # LLVM_CONFIG=llvm-config-3.3 GCC=/app/gcc-bin/bin/gcc-4.6-patch CC=/app/gcc-bin/bin/gcc-4.6-patch CXX=/app/gcc-bin/bin/g++-4.6-patch \
        # CFLAGS="-I /app/gcc-bin/lib/gcc/x86_64-unknown-linux-gnu/4.6.4/plugin/include -I /app/gcc-bin/lib/gcc/x86_64-unknown-linux-gnu/4.6.4/include -I /app/gcc-build/gmp" \
        # CPPFLAGS="-I /app/gcc-bin/lib/gcc/x86_64-unknown-linux-gnu/4.6.4/plugin/include -I /app/gcc-bin/lib/gcc/x86_64-unknown-linux-gnu/4.6.4/include -I /app/gcc-build/gmp" \
        make -j$(nproc)

#emscripten
RUN git clone --depth=1 https://github.com/emscripten-core/emsdk.git
# RUN rm -rf /app/emsdk/upstream/emscripten/system/lib/libc/musl/src/complex
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

#lapack
RUN git clone --depth=1 https://github.com/Reference-LAPACK/lapack.git
COPY vendor/LAPACK/make.inc lapack/make.inc
COPY scripts /app/scripts
RUN cd lapack/SRC && \
    emmake make -j$(nproc) single double

