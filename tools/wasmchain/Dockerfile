FROM ubuntu:18.04 AS build
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends build-essential cmake gnutls-bin wget unzip bash ca-certificates ninja-build python3 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
ARG LLVM_REV="f3da6b7ab5698bb0b9bd208972115dbcbbc59f27"
RUN wget https://github.com/llvm/llvm-project/archive/${LLVM_REV}.zip -O llvm.zip
RUN unzip llvm.zip && \
    mv llvm-project* llvm && \
    rm llvm.zip
RUN cd llvm && \
    mkdir build && \
    cd build && \
    cmake -G Ninja -DCMAKE_INSTALL_PREFIX=/usr/local -DLLVM_ENABLE_PROJECTS='mlir;flang' -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=On ../llvm && \
    ninja -j 6
RUN cd llvm/build && \
    ninja install
    

FROM ubuntu:18.04 AS tool
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

COPY --from=build /usr/local /usr/local
RUN ldconfig

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash && \
    rm -rf /var/lib/apt/lists/*