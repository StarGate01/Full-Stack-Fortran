.PHONY: build dirs

SRCPATH:=/app/gcc/libquadmath
BLDPATH:=/app/libquadmath-build/bin

CC:=emcc
AR:=emar
RANLIB:=emranlib
INCPATH:=-I$(SRCPATH) -I$(SRCPATH)/math -I$(SRCPATH)/printf -I$(SRCPATH)/strtod
CCFLAGS:=--target=wasm32-unknown-emscripten -O3 -c -flto -emit-llvm -m32 -Isrc -Wall -Wno-implicit-function-declaration $(INCPATH)
ARFLAGS:=cr

# Gather sources
CSRCS:=$(wildcard $(SRCPATH)/math/*.c) # $(wildcard $(SRCPATH)/strtod/*.c) # $(wildcard $(SRCPATH)/printf/*.c)

CBCS:=$(CSRCS:$(SRCPATH)/%=$(BLDPATH)/%.bc)
LIB:=$(BLDPATH)/libquadmath.a

# Control targets
build: dirs $(LIB)

dirs:
	cd $(SRCPATH) && \
	find . -type d -exec mkdir -p -- $(BLDPATH)/{} \;

# Regular C
$(BLDPATH)/%.c.bc: $(SRCPATH)/%.c
	$(CC) $(CCFLAGS) -o $@ $< 

# Archive
$(LIB): $(CBCS) $(FBCS)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@
