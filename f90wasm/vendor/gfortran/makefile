.PHONY: build

SRCPATH:=/app/gcc/libgfortran
BLDPATH:=/app/gcc-build/bin

FC:=emfc.sh
CC:=emcc
AR:=emar
RANLIB:=emranlib
INCPATH:=-I$(SRCPATH) -I$(SRCPATH)/config -I$(SRCPATH)/.. -I$(SRCPATH)/../.. -I$(SRCPATH)/../libgcc -I$(SRCPATH)/../gcc 
FCFLAGS:=-O3 -Wall -I$(BLDPATH) -fallow-leading-underscore $(INCPATH)
CCFLAGS:=--target=wasm32-unknown-emscripten -O3 -c -flto -emit-llvm -m32 -Isrc -Wall -Wno-implicit-function-declaration $(INCPATH)
ARFLAGS:=cr

# Gather sources
FSRCS:=$(wildcard $(SRCPATH)/generated/*.F90) $(wildcard $(SRCPATH)/intrinsics/*.F90) \
	$(wildcard $(SRCPATH)/generated/*.f90) $(wildcard $(SRCPATH)/intrinsics/*.f90)
CSRCS:=$(filter-out $(wildcard $(SRCPATH)/runtime/*_inc.c),$(wildcard $(SRCPATH)/runtime/*.c))  \
	$(filter-out $(wildcard $(SRCPATH)/intrinsics/*_inc.c),$(wildcard $(SRCPATH)/intrinsics/*.c)) \
	$(wildcard $(SRCPATH)/generated/*.c) $(wildcard $(SRCPATH)/io/*.c) \
	$(SRCPATH)/fmain.c 

FBCS:=$(FSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
CBCS:=$(CSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
LIB:=$(BLDPATH)/libgfortran.a


# Control targets
build: dirs $(LIB)

dirs:
	cd $(SRCPATH) && \
	find . -type d -exec mkdir -p -- $(BLDPATH)/{} \;


# Compile sources
$(BLDPATH)/%.f90.bc: $(SRCPATH)/%.f90
	$(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<
$(BLDPATH)/%.F90.bc: $(SRCPATH)/%.F90
	$(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<
$(BLDPATH)/%.f.bc: $(SRCPATH)/%.f
	$(FC) $(FCFLAGS) $(MODDIRS) -J $(dir $@) -o $@ -c $<
$(BLDPATH)/%.c.bc: $(SRCPATH)/%.c
	$(CC) $(CCFLAGS) -o $@ $< 

# Archive objects
$(LIB): $(CBCS) $(FBCS)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

