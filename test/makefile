.PHONY: build clean

OPT=-O3

FC:=flang-7
LLC:=llc-8
WLD:=emcc
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm -m32 $(OPT) -c -S -flto -Mpreprocess -Mfreeform -Mextend -I/usr/lib/x86_64-linux-gnu/fortran/flang-mod-34 -nomp
LLCFLAGS:=-march=wasm32 -filetype=obj $(OPT)
WLDFLAGS:=$(OPT) --cache /project/bin/.emscripten_cache -s EXPORTED_FUNCTIONS='["_test_add_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).js
FSRCS:=$(wildcard $(SRCPATH)/*.f90)
FLLS:=$(FSRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.ll)

build: clean $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)

$(FLLS): $(BLDPATH)/%.ll: $(SRCPATH)/%.f90
	mkdir -p $(BLDPATH)
	$(info Compiling fortran to intermediate: $<)
	$(FC) $(FCFLAGS) $< -o $@

$(BIN): $(FLLS)
	mkdir -p $(BLDPATH)
	$(info Linking)
	$(WLD) $(WLDFLAGS) -o $(BIN) $(FLLS)
	
clean:
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.o
