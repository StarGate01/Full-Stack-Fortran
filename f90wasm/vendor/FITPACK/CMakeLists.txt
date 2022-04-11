include(CMakeForceCompiler)

cmake_minimum_required(VERSION 3.0.0)

set(CMAKE_SYSTEM_NAME Generic)
CMAKE_FORCE_Fortran_COMPILER(/app/scripts/emfc.sh gfortran)  # Is there a better way to do this?

enable_language(Fortran)

set(fitpack_SRC
    fitpack/bispev.f
    fitpack/curfit.f
    fitpack/fpback.f
    fitpack/fpbisp.f
    fitpack/fpchec.f
    fitpack/fpcurf.f
    fitpack/fpbspl.f
    fitpack/fpdisc.f
    fitpack/fpgivs.f
    fitpack/fpknot.f
    fitpack/fporde.f
    fitpack/fprank.f
    fitpack/fprati.f
    fitpack/fprota.f
    fitpack/fpsurf.f
    fitpack/parder.f
    fitpack/splder.f
    fitpack/splev.f
    fitpack/surfit.f
)

add_library(fitpack STATIC 
            ${fitpack_SRC}
)