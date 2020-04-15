.PHONY: build clean

OPT:=-O3

FC:=f18
WLD:=emcc
WLDS:=/app/scripts/shim.sh
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm $(OPT) -c -S -flto -m32 -Mfree -Mextend -module bin -I/app/pgi/linux86-64-llvm/19.10/include
WLDFLAGS:=$(OPT) --cache /project/bin/.emscripten_cache -s EXPORTED_FUNCTIONS='["___test_MOD_add"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).js
FSRCS:=$(wildcard $(SRCPATH)/*.f90)
ASRC:=$(BLDPATH)/$(NAME).f90
FLLPRE:=$(BLDPATH)/$(NAME).llpre
FLLPOST:=$(BLDPATH)/$(NAME).ll

build: $(BLDPATH) $(BIN)
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
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.llpre *.o *.f90
