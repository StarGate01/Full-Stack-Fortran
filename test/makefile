.PHONY: build clean

FC:=flang
FCFLAGS:=--target=wasm32 -nostdlib -Wl,--no-entry -Wl,--export-all -i8 -r8 -noFlangLibs -nomp -Mextend -Mfreeform -fno-fortran-main

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).wasm
SRCS:=$(wildcard $(SRCPATH)/*.f90)


build: $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)/

$(BIN): $(SRCS)
	mkdir -p $(BLDPATH)
	$(info Compiling and linking)
	$(FC) -o $(BIN) $(SRCS)
	

clean:
	rm -f $(BIN)