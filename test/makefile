.PHONY: build info clean

# String operation helpers
null  :=
space := $(null) #
comma := ,
define uniq =
  $(eval seen :=)
  $(foreach _,$1,$(if $(filter $_,${seen}),,$(eval seen += $_)))
  ${seen}
endef
rwildcard=$(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

# Path setup
BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

# Build options
OPT:=-O0
DEBUG:=-g

# Compilers and flags
FC:=/app/scripts/emfc.sh
CC:=emcc
WLD:=emcc
FCFLAGS:=$(DEBUG) $(OPT) -Wall -I$(BLDPATH)
CCFLAGS:=$(DEBUG) --target=wasm32-unknown-emscripten $(OPT) -c -flto -emit-llvm -m32 -s EXPORT_ALL=1 -Isrc -Wall \
	-Isrc/gcc -Isrc/gcc/libgfortran -Isrc/gcc/libgcc -Isrc/gcc/gcc -Isrc/gcc/libgfortran/config
EMSFLAGS:=$(OPT) $(DEBUG) -m32 --cache /project/bin/.emscripten_cache -Wall
WLDFLAGS:=-s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s MODULARIZE=1 -s ERROR_ON_UNDEFINED_SYMBOLS=0

# Project config
NAME:=assembly
EXPORTS:=test_MOD_add test_MOD_eigen
LIBS:=/app/lapack/liblapack.a /app/lapack/librefblas.a
EXPORTSD:=$(EXPORTS:%="___%")
# EXPORTFLAGS:=-s LINKABLE=1 -s EXPORT_ALL=1
EXPORTFLAGS:=-s EXPORTED_FUNCTIONS='[$(subst $(space),$(comma),$(strip $(EXPORTSD)))]'

# Define sources and dependencies
# IOlib
FSRCS_IOLIB:=$(SRCPATH)/iolib/iolib_types.f90 $(SRCPATH)/iolib/iolib.f90
CSRCS_IOLIB:=$(SRCPATH)/iolib/iolib.c
MOD_IOLIB:=$(CSRCS_IOLIB:$(SRCPATH)/%=$(BLDPATH)/%.bc)
# gfortran shim
FSRCS_GFORTRAN:= 
CSRCS_GFORTRAN:=$(SRCPATH)/gcc/libgfortran/intrinsics/string_intrinsics.c $(SRCPATH)/gcc/libgfortran/io/transfer.c $(SRCPATH)/gcc/libgfortran/runtime/compile_options.c \
	$(SRCPATH)/gcc/libgfortran/runtime/main.c $(SRCPATH)/gcc/libgfortran/runtime/environ.c $(SRCPATH)/gcc/libgfortran/runtime/stop.c $(SRCPATH)/gcc/libgfortran/io/unit.c \
	$(SRCPATH)/gcc/libgfortran/io/unix.c $(SRCPATH)/gcc/libgfortran/io/fbuf.c $(SRCPATH)/gcc/libgfortran/runtime/string.c $(SRCPATH)/gcc/libgfortran/io/list_read.c \
	$(SRCPATH)/gcc/libgfortran/io/read.c $(SRCPATH)/gcc/libgfortran/io/format.c $(SRCPATH)/gcc/libgfortran/io/lock.c $(SRCPATH)/gcc/libgfortran/runtime/error.c \
	$(SRCPATH)/gcc/libgfortran/runtime/memory.c $(SRCPATH)/gcc/libgfortran/io/write.c $(SRCPATH)/gcc/libgfortran/io/open.c $(SRCPATH)/gcc/libgfortran/intrinsics/random.c \
	$(SRCPATH)/gcc/libgfortran/runtime/backtrace.c $(SRCPATH)/gcc/libgfortran/io/size_from_kind.c  $(SRCPATH)/gcc/libgfortran/runtime/fpu.c  $(SRCPATH)/gcc/libgfortran/intrinsics/c99_functions.c
MOD_GFORTRAN:=$(CSRCS_GFORTRAN:$(SRCPATH)/%=$(BLDPATH)/%.bc)
# User project files
FSRCS_USER:=$(SRCPATH)/test.f90
CSRCS_USER:=
MOD_USER:=$(BLDPATH)/test.f90.bc
# Define dependencies
$(FSRCS_USER): $(MOD_IOLIB) $(MOD_GFORTRAN)
# Aggregate sources
FSRCS:=$(FSRCS_IOLIB) $(FSRCS_GFORTRAN) $(FSRCS_USER)
CSRCS:=$(CSRCS_IOLIB) $(CSRCS_GFORTRAN) $(CSRCS_USER)
MODDIRS:=-Ibin -Ibin/iolib

# Create file lists
FBCS:=$(FSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
CBCS:=$(CSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
BIN:=$(BLDPATH)/$(NAME).js
OBJ:=$(BLDPATH)/$(NAME).bc

# Control targets
build: clean $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)

info:
	$(info F Sources:	$(FSRCS))
	$(info C Sources:	$(CSRCS))
	$(info Binaries:	$(FBCS) $(FBCS))
	$(info Moduledirs:	$(MODDIRS))
	$(info )

clean:
	$(shell mkdir -p $(BLDPATH))
	cd $(BLDPATH) && rm -rf iolib gfortran *.bc *.ll *.llpre *.mod *wasm *.html *.js

# Fortran 95 and above
$(BLDPATH)/%.f90.bc: $(SRCPATH)/%.f90
	$(shell mkdir -p $(dir $@))
	KEEP_TEMPS=1 $(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<

# Legacy Fortran
$(BLDPATH)/%.f.bc: $(SRCPATH)/%.f
	$(shell mkdir -p $(dir $@))
	$(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<

# Regular C
$(BLDPATH)/%.c.bc: $(SRCPATH)/%.c
	$(shell mkdir -p $(dir $@))
	$(CC) $(CCFLAGS) -o $@ $< 
	$(CC) $(CCFLAGS) -emit-llvm -S -flto -m32 -o $@.ll $< 

# Javascript finish
$(OBJ): $(CBCS) $(FBCS) $(LIBS)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) -r -o $@ $^

$(BIN): $(OBJ)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) $(WLDFLAGS) $(EXPORTFLAGS) -o $@ $< 
	
