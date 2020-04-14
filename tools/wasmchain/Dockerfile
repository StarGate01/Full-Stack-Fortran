FROM ubuntu:14.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends wget bash make \
        gfortran-4.6 g++-4.6 gcc-4.6 gcc-4.6-plugin-dev llvm-3.3-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN ln -s $(which llvm-config-3.3) /usr/bin/llvm-config && \
    ln -s $(which g++-4.6) /usr/bin/g++
RUN wget http://llvm.org/releases/3.3/dragonegg-3.3.src.tar.gz && \
    tar xzf dragonegg-3.3.src.tar.gz && \
    cd dragonegg-3.3.src && \
    GCC=gcc-4.6 make