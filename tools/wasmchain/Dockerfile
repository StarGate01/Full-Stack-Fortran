FROM ubuntu:19.10
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends \
        build-essential python3 python3-distutils \
        wget git gnutls-bin bash make ca-certificates xz-utils \
        gfortran-8 g++-8 gcc-8 gcc-8-plugin-dev llvm-6.0-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
# RUN wget http://www.cmake.org/files/v3.4/cmake-3.4.3.tar.gz -O cmake.tar.gz
# RUN tar -xzf cmake.tar.gz && \
#     mv cmake-* cmake && \
#     cd cmake && \
#     ./configure --prefix=/usr/local && \
#     make -j4 && \
#     make install

# ARG LLVM_REV="465d2ab836ab3907916652ac93a715364b53e0b2"
# RUN wget https://github.com/llvm/llvm-project/archive/${LLVM_REV}.tar.gz -O llvm.tar.gz
# RUN tar -xzf llvm.tar.gz && \
#     mv llvm-project* llvm
# RUN cd llvm && \
#     mkdir build && \
#     cd build && \
#     CC=gcc-4.6 CXX=g++-4.6 cmake -G Ninja -DCMAKE_INSTALL_PREFIX=/usr/local -DLLVM_ENABLE_PROJECTS='lld;clang' -DLLVM_TARGETS_TO_BUILD= -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly \
#         -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=On ../llvm && \
#     ninja -j 4 && \
#     ninja install


# RUN wget https://llvm.org/releases/3.3/dragonegg-3.3.src.tar.gz && \
#     tar xzf dragonegg-3.3.src.tar.gz && \
#     cd dragonegg-3.3.src && \
#     LLVM_CONFIG=llvm-config-3.3 GCC=gcc-4.6 CC=gcc-4.6 CXX=g++-4.6 make
RUN git clone https://github.com/ranjani141/dragonegg.git && \
    cd dragonegg && \
    LLVM_CONFIG=llvm-config-6.0 GCC=gcc-8 CC=gcc-8 CXX=g++-8 make

RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"

COPY scripts /app/scripts

# ARG BYEN_REV="dfb2f27d0123ad62b44914fd0210db1d21cfc91e"
# RUN wget https://github.com/WebAssembly/binaryen/archive/${LLVM_REV}.tar.gz -O binaryen.tar.gz
# RUN tar -xzf binaryen.tar.gz && \
#     mv binaryen-project* binaryen
# RUN cd binaryen && \
#     mkdir build && \
#     cd build && \
#     CC=gcc-5 CXX=g++-5 cmake -G Ninja -DCMAKE_INSTALL_PREFIX=/usr/local .. && \
#     ninja -j 4 && \
#     ninja install

# RUN curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -

# ARG EMS_VERSION="1.32.3"
# RUN wget https://github.com/emscripten-core/emscripten/archive/${EMS_VERSION}.tar.gz -O emscripten.tar.gz && \
#     tar -xzf emscripten.tar.gz && \
#     mv emscripten-* emscripten


# # RUN cd emscripten-${EMS_VERSION} && \
# #     ./emsdk install ${EMS_VERSION} && \
# #     ./emsdk activate ${EMS_VERSION}

# RUN touch /app/.emscripten
# ENV EM_CONFIG=/app/.emscripten
