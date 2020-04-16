.PHONY: build clean

OPT:=-O3

FC:=flang
WLD:=emcc
WLDS:=/app/scripts/shim.sh
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm $(OPT) -c -S -flto -m32 -Mfree -Mextend -module bin -I/opt/AMD/aocc-compiler-2.1.0/include
WLDFLAGS:=$(OPT) --cache /project/bin/.emscripten_cache -s EXPORTED_FUNCTIONS='["_test_add_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' --llvm-opts "['$(OPT)']" --closure 1

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).js
FSRCS:=$(wildcard $(SRCPATH)/*.f90)
ASRC:=$(BLDPATH)/$(NAME).f90
FLLPRE:=$(BLDPATH)/$(NAME).llpre
FLLPOST:=$(BLDPATH)/$(NAME).ll

build: clean $(BLDPATH) $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)

$(ASRC): $(FSRCS)
	cat $^ > $@

$(FLLPRE): $(ASRC)
	$(FC) $(FCFLAGS) $< -o $@

$(FLLPOST): $(FLLPRE)
	$(WLDS) $< $@

$(BIN): $(FLLPOST)
	$(WLD) $(WLDFLAGS) -o $@ $<

$(BLDPATH):
	mkdir -p $(BLDPATH)

clean:
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.llpre *.o *.f90 *.mod
