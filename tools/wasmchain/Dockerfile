FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive TZ=Europe/Berlin

RUN apt-get update && \
    apt-get -y install --no-install-recommends make bash flang-7 libflang0d-7 llvm-10 llvm-10-tools llvm-10-runtime lld-10 && \
    rm -rf /var/lib/apt/lists/*