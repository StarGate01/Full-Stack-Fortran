FTN_INTRINSICS_DESC_DEP:=red.c red_all.c red_any.c red_count.c red_findloc.c red_iany.c red_maxloc.c red_minloc.c red_maxval.c red_minval.c red_norm2.c red_norm2_stride1.c red_sum.c reduct.c scatter.c scatter_maxval.c scatter_minval.c
FTN_INTRINSICS_DESC_DEP_S:=$(FTN_INTRINSICS_DESC_DEP:%=$(SRCPATH)/flang/runtime/flang/%)
FTN_INTRINSICS_DESC_DEP_LL:=$(FTN_INTRINSICS_DESC_DEP_S:$(SRCPATH)/%.c=$(BLDPATH)/%.c.ll)
FTN_INTRINSICS_DESC_DEP_LL_I8:=$(FTN_INTRINSICS_DESC_DEP_LL:%.ll=%.i8.ll)