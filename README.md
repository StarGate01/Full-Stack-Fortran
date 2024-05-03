[![Build Status](https://drone-github.chrz.de/api/badges/StarGate01/Full-Stack-Fortran/status.svg)](https://drone-github.chrz.de/StarGate01/Full-Stack-Fortran)
[![Docker Pulls](https://img.shields.io/docker/pulls/stargate01/f90wasm)](https://hub.docker.com/r/stargate01/f90wasm)

# Full-Stack-Fortran

A toolchain to compile Fortran to Webassembly, and a few projects using it.

Blog post: https://chrz.de/2020/04/21/fortran-in-the-browser/

Docker image: https://hub.docker.com/r/stargate01/f90wasm

### 2024 Update Note

This project was originally published in 2020. Today, in 2024, the FORTRAN compiler landscape has changed quite a bit – while FORTRAN still does not enjoy first-class WebAssembly support in the LLVM toolchain, it is now possible to set up a much more sane toolchain by applying a few patches to LLVM.

[Dr George W Stagg has published an excellent writeup](https://gws.phd/posts/fortran_wasm/) of his LLVM modifications, and the application of the resulting toolchain to several real-world projects. Specifically, the [WebR](https://github.com/r-wasm/webr) project and a cool BLAS-based in-browser digit classifier.

## Usage

Run `./f90wasm/build.sh` to build the docker image locally. A pre-compiled image is available on the [Docker Hub](https://hub.docker.com/r/stargate01/f90wasm).

Run `./test/test.sh` to compile the test application, and host it on http://localhost:8080 . Use `./test/clean.sh` to remove compiled test binaries.

## Thanks to

 - http://www.netlib.org/lapack/
 - https://gcc.gnu.org/
 - https://llvm.org/
 - https://emscripten.org/
 - https://github.com/smikes/femscripten
 - [Aman Gupta Karmani (tmm1)](https://github.com/tmm1) for helping to update the Emscripten version used (#10, #12, #13)

 ## TLDR

 ![Toolchain Flowchart](toolchain.png)
