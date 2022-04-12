#include <stdio.h>
#include <complex.h>

extern void canary_dotest_c(void);

int iolib_test (int a) {
    printf("Hello from C!\n");
    canary_dotest_c();
    return a * 10;
}

