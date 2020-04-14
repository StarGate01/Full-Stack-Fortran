.PHONY: build clean

FC:=flang-7
LLC:=llc-8
WLD:=emcc
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm -O3 -c -S -flto -m32 -Mfreeform -Mextend -I/usr/lib/x86_64-linux-gnu/fortran/flang-mod-34
LLCFLAGS:=-march=wasm32 -filetype=obj -O3
WLDFLAGS:=-O3 --cache /project/bin/.emscripten_cache -s LINKABLE=1 -s EXPORT_ALL=1

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).html
SRCS:=$(wildcard $(SRCPATH)/*.f03)
LLS:=$(SRCS:$(SRCPATH)/%.f03=$(BLDPATH)/%.ll)
OBJS:=$(SRCS:$(SRCPATH)/%.f03=$(BLDPATH)/%.o)


build: clean $(BIN)

$(LLS): $(BLDPATH)/%.ll: $(SRCPATH)/%.f03
	mkdir -p $(BLDPATH)
	$(info Compiling to intermediate: $<)
	$(FC) $(FCFLAGS) $< -o $@

$(OBJS): $(BLDPATH)/%.o: $(BLDPATH)/%.ll
	mkdir -p $(BLDPATH)
	$(info Assembling to object: $<)
	$(LLC) $(LLCFLAGS) $< -o $@

$(BIN): $(OBJS)
	mkdir -p $(BLDPATH)
	$(info Linking)
	$(WLD) $(WLDFLAGS) -o $(BIN) $(OBJS)

clean:
	find . ! -name '.emscripten_cache' -type f -exec rm -rf {} +