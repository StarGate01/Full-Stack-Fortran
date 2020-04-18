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

# Path setup
BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

# Build options
OPT:=3
DEBUG:=

# Compilers and flags
FC:=/app/scripts/gfortran.sh
CC:=emcc
WLD:=emcc
FCFLAGS:=$(DEBUG) -emit-llvm -S -flto -m32 -fverbose-asm -nostdlib -fplugin=/app/dragonegg-3.3.src/dragonegg.so -O$(OPT) \
	-fplugin-arg-dragonegg-llvm-ir-optimize=$(OPT) -Wall -I$(BLDPATH)
CCFLAGS:=$(DEBUG) --target=wasm32 -O$(OPT) -c -flto  -emit-llvm -m32 -s EXPORT_ALL=1 -Isrc -Wall
WLDFLAGS:=-O$(OPT) $(DEBUG) -m32 --cache /project/bin/.emscripten_cache -Wall \
	-s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' --closure 1 -s MODULARIZE=1

# Project config
NAME:=assembly
EXPORTS:=test_MOD_add test_MOD_eigen
LIBS:=/app/lapack/liblapack.a /app/lapack/librefblas.a
EXPORTSD:=$(EXPORTS:%="___%")
EXPORTFLAGS:=-s EXPORTED_FUNCTIONS='[$(subst $(space),$(comma),$(strip $(EXPORTSD)))]'

# Define sources and dependencies
# IOlib
FSRCS_IOLIB:=$(SRCPATH)/iolib/iolib_types.f90 $(SRCPATH)/iolib/iolib.f90
CSRCS_IOLIB:=$(SRCPATH)/iolib/iolib.c
MOD_IOLIB:=$(BLDPATH)/iolib/iolib.c.bc
# User project files
FSRCS_USER:=$(SRCPATH)/test.f90
CSRCS_USER:=
MOD_USER:=$(BLDPATH)/test.f90.bc
# Define dependencies
$(FSRCS_USER): $(MOD_IOLIB)
# Aggregate sources
FSRCS:=$(FSRCS_IOLIB) $(FSRCS_USER)
CSRCS:=$(CSRCS_IOLIB) $(CSRCS_USER)
MODDIRS:=-Ibin -Ibin/iolib

# Create file lists
FBCS:=$(FSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
CBCS:=$(CSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
BIN:=$(BLDPATH)/$(NAME).js
# Keep intermediate files?
# .PRECIOUS: $(FLLSPRE) $(FLLS)

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
	rm -rf $(BLDPATH)

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

# Javascript finish
$(BIN): $(CBCS) $(FBCS) $(LIBS)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(WLDFLAGS) $(EXPORTFLAGS) -o $@ $< 
	
