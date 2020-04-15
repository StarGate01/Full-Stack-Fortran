.PHONY: build clean

rwildcard=$(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

OPT:=-O3

FC:=flang-7
CC:=emcc
WLD:=emcc
WLDS:=/app/scripts/shim.sh
FCFLAGS:=--target=wasm32 -nostdlib -emit-llvm -c -S -flto $(OPT) -m32 -Mfreeform -Mextend -I/usr/lib/x86_64-linux-gnu/fortran/flang-mod-34
CCFLAGS:=--target=wasm32 $(OPT) -c -S -flto -m32 -s LINKABLE=1 -s EXPORT_ALL=1 -Isrc/lib/flang/include -Isrc/lib/flang/runtime -Isrc/lib/flang/runtime/include -Isrc/lib/include
EMSFLAGS:=$(OPT) -m32 --cache /project/bin/.emscripten_cache --emit-symbol-map --llvm-opts "['$(OPT)']" --closure 1
WLDFLAGS:=-s EXPORTED_FUNCTIONS='["_test_add_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'

BLDPATH:=bin
SRCPATH:=src
STAPATH:=static

NAME:=testlib
BIN:=$(BLDPATH)/$(NAME).js
FSRCS:=$(call rwildcard,$(SRCPATH),*.f90)
LIBSRCS:=$(call rwildcard,$(SRCPATH)/lib,*.c)
ASRC:=$(BLDPATH)/$(NAME).f90
ALLSTAT:=$(BLDPATH)/$(NAME).bc
FLLPRE:=$(BLDPATH)/$(NAME).llpre
FLLPOST:=$(BLDPATH)/$(NAME).ll
LIBFLLS:=$(LIBSRCS:$(SRCPATH)/lib/%.c=$(BLDPATH)/lib/%.ll)
# EXT:=$(SRCPATH)/externals.f90

build: clean $(BLDPATH)/lib $(BIN)
	cp -f $(STAPATH)/* $(BLDPATH)

$(ASRC): $(FSRCS)
	cat $^ > $@

# $(EXT): $(LIBSRCS)
	# echo "" > $(EXT)
	# for f in $^; do cproto -s $$f >> $(EXT); done

#$(EXT)

$(FLLPRE): $(ASRC)
	$(FC) $(FCFLAGS) $< -o $@

$(FLLPOST): $(FLLPRE)
	$(WLDS) $< $@

$(LIBFLLS): $(BLDPATH)/lib/%.ll: $(SRCPATH)/lib/%.c
	$(shell mkdir -p $(dir $@))
	$(CC) $(CCFLAGS) $< -o $@

$(ALLSTAT): $(LIBFLLS) $(FLLPOST)
	$(WLD) $(EMSFLAGS) -o $@ $^

$(BIN): $(ALLSTAT)
	$(WLD) $(EMSFLAGS) $(WLDFLAGS) -o $@ $<

$(BLDPATH)/lib:
	mkdir -p $(BLDPATH)/lib

clean:
	cd $(BLDPATH) && rm -f *.html *.js *.wasm *.ll *.llpre *.o *.f90 && rm -rf lib
