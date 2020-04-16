.PHONY: build clean

OPT:=-O3
FC:=flang-7
CC:=emcc
WLD:=emcc

FCFLAGS:=--target=wasm32 -nostdlib $(OPT) -m32 -Mfreeform -Mextend -Ibin -Jbin -Mpreprocess
CCFLAGS:=--target=wasm32 $(OPT) -c -S -flto -m32 -s LINKABLE=1 -s EXPORT_ALL=1 -Isrc/lib -Isrc
EMSFLAGS:=$(OPT) -m32 --cache /project/bin/.emscripten_cache --llvm-opts "['$(OPT)']" --closure 1
WLDFLAGS:=-s EXPORTED_FUNCTIONS='["_test_add_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'


BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=assembly
BIN:=$(BLDPATH)/$(NAME).js
OBJ:=$(BLDPATH)/$(NAME).bc

FSRCSNAMES:=iso_c_binding.f90 c_interface_module.f90 test.f90
CSRCSNAMES:=

FSRCS:=$(FSRCSNAMES:%=$(SRCPATH)/%)
CSRCS:=$(wildcard $(SRCPATH)/*.c)

FLLS:=$(FSRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.f90.ll)
CLLS:=$(CSRCS:$(SRCPATH)/%.c=$(BLDPATH)/%.c.ll)

build: clean $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)


$(BLDPATH)/%.f90.ll: $(SRCPATH)/%.f90
	$(shell mkdir -p $(dir $@))
	$(FC) $(FCFLAGS) -emit-llvm -S -flto -o $@ -c $<

$(BLDPATH)/%.c.ll: $(SRCPATH)/%.c
	$(shell mkdir -p $(dir $@))
	$(CC) $(CCFLAGS) -o $@ $< 

$(OBJ): $(CLLS) $(FLLS)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) -o $@ $^

$(BIN): $(OBJ)
	$(shell mkdir -p $(dir $@))
	$(WLD) $(EMSFLAGS) $(WLDFLAGS) -o $@ $<

clean:
	mkdir -p $(BLDPATH)
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.llpre *.o *.f90 *.bc *.mod
