.PHONY: build info clean purge

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
OPT:=-O3
DEBUG:=

# Compilers and flags
FC:=emfc.sh
CC:=emcc
WLD:=emcc
FCFLAGS:=$(DEBUG) $(OPT) -Wall -I$(BLDPATH) -I$(BLDPATH)/lapack_test
CCFLAGS:=$(DEBUG) --target=wasm32-unknown-emscripten $(OPT) -c -flto -emit-llvm -m32 -Isrc -Wall
EMSFLAGS:=$(OPT) $(DEBUG) -m32 --cache /project/bin/.emscripten_cache -Wall -flto
WLDFLAGS:=-s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s MODULARIZE=1 -s ERROR_ON_UNDEFINED_SYMBOLS=1 -s USE_SDL=2 -s LEGACY_GL_EMULATION=1

# Project config
NAME:=assembly
EXPORTS:=test_MOD_add lapack_prb_MOD_lapack_prb_main test_eigen_MOD_test_eigen_main
LIBS:=/app/lib/libgfortran.a /app/lib/liblapack.a /app/lib/librefblas.a
EXPORTSD:=$(EXPORTS:%="___%")
# EXPORTFLAGS:=-s LINKABLE=1 -s EXPORT_ALL=1
EXPORTFLAGS:=-s EXPORTED_FUNCTIONS='[$(subst $(space),$(comma),$(strip $(EXPORTSD)))]'

# Define sources and dependencies
# IOlib
FSRCS_IOLIB:=$(SRCPATH)/iolib/iolib.f90
CSRCS_IOLIB:=$(SRCPATH)/iolib/iolib.c
MOD_IOLIB:=$(CSRCS_IOLIB:$(SRCPATH)/%=$(BLDPATH)/%.bc)
# User project files
FSRCS_USER:=$(SRCPATH)/test.f90 $(SRCPATH)/lapack_test/test_eigen.f90 $(SRCPATH)/lapack_test/lapack_prb.f90
CSRCS_USER:=
MOD_USER:=
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
	find $(SRCPATH) -name "*.mod" -type f -delete
	cd $(BLDPATH) && rm -rf iolib *.bc *.ll *.llpre *.mod *wasm *.html *.js

purge:
	rm -rf $(BLDPATH)

# Fortran 95 and above
$(BLDPATH)/%.f90.bc: $(SRCPATH)/%.f90
	$(shell mkdir -p $(dir $@))
	KEEP_TEMPS=1 $(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<
$(BLDPATH)/%.F90.bc: $(SRCPATH)/%.F90
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
	
