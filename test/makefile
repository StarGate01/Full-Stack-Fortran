.PHONY: build clean

FC:=flang
FCFLAGS:=--target=wasm32 -nostdlib -Wl,--no-entry -Wl,--export-all

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
	rm -rf $(BLDPATH)