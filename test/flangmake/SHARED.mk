SHARED:=ccffinfo.c direct.c error.c go.c ilidir.c llmputil.c mall.c miscutil.c pragma.c rtlRtns.c salloc.c x86.c nmeutil.c
SHARED_S:=$(SHARED:%=$(SRCPATH)/flang/tools/shared/%)
SHARED_LL:=$(SHARED_S:$(SRCPATH)/%.c=$(BLDPATH)/%.c.ll)