.PHONY: build clean

OPT:=-O3
FC:=flang-7
CC:=emcc
WLD:=emcc
WLDS:=/app/scripts/shim.sh

FCFLAGS:=--target=wasm32 -nostdlib $(OPT) -m32 -Mfreeform -Mextend -I/usr/lib/x86_64-linux-gnu/fortran/flang-mod-34
CCFLAGS:=--target=wasm32 $(OPT) -c -S -flto -m32 -s LINKABLE=1 -s EXPORT_ALL=1 -Isrc/lib -Isrc
EMSFLAGS:=$(OPT) -m32 --cache /project/bin/.emscripten_cache --llvm-opts "['$(OPT)']" --closure 1
WLDFLAGS:=-s EXPORTED_FUNCTIONS='["_test_add_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'


BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).js
OBJ:=$(BLDPATH)/$(NAME).bc

FSRCSNAMES:=test.f90
CSRCSNAMES:=f90lib.c

FSRCS:=$(FSRCSNAMES:%=$(SRCPATH)/%)
CSRCS:=$(CSRCSNAMES:%=$(SRCPATH)/%)

FLLS:=$(FSRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.f90.ll)
FMODS:=$(FSRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.mod)
CLLS:=$(CSRCS:$(SRCPATH)/%.c=$(BLDPATH)/%.c.ll)

build: clean $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)

$(BLDPATH)/%.f90.ll: $(BLDPATH)/%.f90.llpre
	$(shell mkdir -p $(dir $@))
	$(WLDS) $< $@

$(BLDPATH)/%.f90.llpre: $(BLDPATH)/%.mod
	$(shell mkdir -p $(dir $@))
	$(FC) $(FCFLAGS) -emit-llvm -S -flto -o $(@:$(BLDPATH)/%.mod=$(SRCPATH)/%.f90) -c $<

$(BLDPATH)/%.mod: $(SRCPATH)/%.f90
	$(shell mkdir -p $(dir $@))
	$(FC) $(FCFLAGS) -o $@ -c $< 

$(BLDPATH)/%.c.ll: $(SRCPATH)/%.c
	$(shell mkdir -p $(dir $@))
	$(CC) $(CCFLAGS) -o $@ $< 

$(OBJ): $(FLLS) $(CLLS)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) -o $@ $^

$(BIN): $(OBJ)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) $(WLDFLAGS) -o $@ $<

clean:
	$(info $(shell ls))
	mkdir -p $(BLDPATH)
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.llpre *.o *.f90 *.bc && rm -rf lib
