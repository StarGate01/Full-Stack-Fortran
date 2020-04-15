.PHONY: build clean

FC:=f18
LLC:=llc-8
WLD:=wasm-ld-8
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm -O3 -c -S -flto -m32 -Mfree -Mextend -module bin
LLCFLAGS:=-march=wasm32 -filetype=obj -O3
WLDFLAGS:=--no-entry --lto-O3 --allow-undefined

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).wasm
SRCS:=$(wildcard $(SRCPATH)/*.f90)
LLS:=$(SRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.ll)
OBJS:=$(SRCS:$(SRCPATH)/%.f90=$(BLDPATH)/%.o)


build: clean $(BIN)

$(LLS): $(BLDPATH)/%.ll: $(SRCPATH)/%.f90
	mkdir -p $(BLDPATH)
	$(FC) $(FCFLAGS) $< -o $@

$(OBJS): $(BLDPATH)/%.o: $(BLDPATH)/%.ll
	mkdir -p $(BLDPATH)
	$(LLC) $(LLCFLAGS) $< -o $@

$(BIN): $(OBJS)
	mkdir -p $(BLDPATH)
	$(WLD) $(WLDFLAGS) -o $(BIN) $(OBJS)
	

clean:
	mkdir -p $(BLDPATH)
	rm -f $(LLS) $(OBJS) $(BIN) $(BLDPATH)/*.tmp*