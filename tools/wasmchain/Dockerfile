FROM ubuntu:18.04 AS build
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends build-essential gnutls-bin cmake xz-utils make wget bash python3 ca-certificates ninja-build && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
ARG LLVM_REV="b43612401077912a577fd9700c7284b6e676f3fe"
RUN wget https://github.com/llvm/llvm-project/archive/${LLVM_REV}.tar.gz -O llvm.tar.gz
RUN tar -xzf llvm.tar.gz && \
    mv llvm-project* llvm
RUN cd llvm && \
    mkdir build && \
    cd build && \
    cmake -G Ninja -DCMAKE_INSTALL_PREFIX=/usr/local -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF \
        -DLLVM_ENABLE_PROJECTS='mlir;flang' -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=On ../llvm && \
    ninja -j 6 && \
    ninja install

COPY vendor/pgilinux-2019-1910-x86-64.tar.gz pgilinux-2019-1910-x86-64.tar.gz
RUN mkdir pgilinux && \
    tar xvzf pgilinux-2019-1910-x86-64.tar.gz -C pgilinux && \
    cd pgilinux && \
    PGI_SILENT=true PGI_ACCEPT_EULA=accept PGI_INSTALL_DIR=/app/pgi PGI_INSTALL_TYPE=single \
        PGI_INSTALL_NVIDIA=false PGI_INSTALL_JAVA=false PGI_INSTALL_MPI=false PGI_MPI_GPU_SUPPORT=false \
        ./install


FROM ubuntu:18.04 AS tool
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

COPY --from=build /usr/local /usr/local
COPY --from=build /app/pgi /app/pgi
ENV PGI=/app/pgi LM_LICENSE_FILE=/app/pgi/license.dat

RUN apt-get update && \
    apt-get -y install --no-install-recommends build-essential cmake xz-utils python3 python3-distutils git make gnutls-bin bash ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}

COPY scripts /app/scripts
ENV PATH="/app/pgi/linux86-64/19.10/bin:/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"