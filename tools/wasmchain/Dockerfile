FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash flang-7 libflang0d-7 llvm-7 llvm-7-tools llvm-7-runtime lld-7 && \
    rm -rf /var/lib/apt/lists/*