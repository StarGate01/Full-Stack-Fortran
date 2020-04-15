FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends wget cmake git gnutls-bin python3 python3-distutils bash make ca-certificates xz-utils
RUN apt-get -y install --no-install-recommends gfortran-8 g++-8 gcc-8 gcc-8-plugin-dev
RUN apt-get -y install --no-install-recommends llvm-6.0-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/xiangzhai/dragonegg && \
    cd dragonegg && \
    LLVM_CONFIG=llvm-config-6.0 GCC=gcc-8 CXX=g++-8 make

# RUN git clone https://github.com/emscripten-core/emsdk.git
# ARG EMS_VERSION="1.39.12"
# RUN cd emsdk && \
#     ./emsdk install ${EMS_VERSION} && \
#     ./emsdk activate ${EMS_VERSION}
