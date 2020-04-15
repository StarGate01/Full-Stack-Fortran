FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends cmake wget python3 python3-distutils make bash git ca-certificates \
        xz-utils llvm-8 llvm-8-tools llvm-8-runtime lld-7  && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN git clone https://github.com/emscripten-core/emsdk.git
RUN cd emsdk && \
    ./emsdk install 1.39.12 && \
    ./emsdk activate 1.39.12

RUN apt-get update && \
    apt-get -y install --no-install-recommends python3-pip  && \
    rm -rf /var/lib/apt/lists/*

RUN wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh && \
    bash miniconda.sh -b -p /app/miniconda && \
    rm miniconda.sh

RUN conda create -n lf && \
    conda activate lf && \
    conda install lfortran -c conda-forge