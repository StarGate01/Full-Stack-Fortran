.PHONY: flang

include flangmake/FTN_INTRINSICS_DESC_DEP.mk flangmake/FTN_INTRINSICS_DESC_INDEP.mk flangmake/FTN_SUPPORT_DESC_DEP.mk flangmake/FTN_SUPPORT_DESC_INDEP.mk flangmake/SHARED.mk

FLANGRTSRC:=$(SRCPATH)/flang/runtime/flang
FLANGRTBLD:=$(BLDPATH)/flang/runtime/flang

$(FTN_INTRINSICS_DESC_DEP_LL_I8) $(FTN_SUPPORT_DESC_DEP_LL_I8): $(FLANGRTBLD)/%.c.i8.ll: $(FLANGRTSRC)/%.c
	$(shell mkdir -p $(dir $@))
	$(CC) $(CCFLAGS) -DDESC_I8 -o $@ $< 

ERRMSGPATH:=$(SRCPATH)/flang/include/flang/Error
ERRMSG:=errmsg-accel.n errmsg-common.n errmsg-hammer.n errmsg.n
ERRMSG_S:=$(ERRMSG:%=$(ERRMSGPATH)/%)
ERRMSGB:=errmsgdf.h new_errmsg.n errmsg.rst
ERRMSGPATH:=$(BLDPATH)/flang/utils/errmsg
ERRMSGB_O:=$(ERRMSGB:%=$(ERRMSGPATH)/%)

errmsg: $(ERRMSGB_O)

$(ERRMSGB_O): $(ERRMSG_S)
	$(shell mkdir -p $(dir $@))
	/app/scripts/errmsggen $^ -o $(ERRMSGPATH)/errmsgdf.h -e $(ERRMSGPATH)/new_errmsg.n -s $(ERRMSGPATH)/errmsg.rst

flang: errmsg $(SHARED_LL) $(FTN_INTRINSICS_DESC_DEP_LL) $(FTN_INTRINSICS_DESC_DEP_LL_I8) $(FTN_INTRINSICS_DESC_INDEP_LL) $(FTN_SUPPORT_DESC_DEP_LL) $(FTN_SUPPORT_DESC_DEP_LL_I8) $(FTN_SUPPORT_DESC_INDEP_LL)