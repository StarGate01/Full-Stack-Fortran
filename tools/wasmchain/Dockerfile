FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends cmake wget make bash git ca-certificates \
        xz-utils llvm-8 llvm-8-tools llvm-8-runtime lld-7  && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN wget --quiet https://repo.anaconda.com/archive/Anaconda3-2020.02-Linux-x86_64.sh -O anaconda.sh && \
    /bin/bash anaconda.sh -b -p /app/conda && \
    rm anaconda.sh && \
    ln -s /app/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /app/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc && \
    find /app/conda/ -follow -type f -name '*.a' -delete && \
    find /app/conda/ -follow -type f -name '*.js.map' -delete && \
    /app/conda/bin/conda clean -afy
ENV PATH /app/conda/bin:$PATH
RUN conda --version

RUN conda create -n lf
RUN bash -i -c "conda install lfortran -c conda-forge"


RUN git clone https://github.com/emscripten-core/emsdk.git
ARG EMS_VERSION="1.39.12"
RUN cd emsdk && \
    ./emsdk install ${EMS_VERSION} && \
    ./emsdk activate ${EMS_VERSION}
ENV PATH="/app/emsdk:/app/emsdk/node/12.9.1_64bit/bin:/app/emsdk/upstream/emscripten:${PATH}"