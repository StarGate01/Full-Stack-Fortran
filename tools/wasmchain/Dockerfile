FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN echo "deb http://archive.ubuntu.com/ubuntu/ trusty main restricted universe" >> /etc/apt/sources.list && \
    echo "deb http://security.ubuntu.com/ubuntu/ trusty-security main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install --no-install-recommends wget bash make ca-certificates \
        gfortran-4.6 g++-4.6 gcc-4.6 gcc-4.6-plugin-dev llvm-3.3-dev lld-6.0 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN ln -s $(which llvm-config-3.3) /usr/bin/llvm-config && \
    ln -s $(which g++-4.6) /usr/bin/g++
RUN wget https://llvm.org/releases/3.3/dragonegg-3.3.src.tar.gz && \
    tar xzf dragonegg-3.3.src.tar.gz && \
    cd dragonegg-3.3.src && \
    GCC=gcc-4.6 make
