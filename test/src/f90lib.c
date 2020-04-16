#include <unistd.h>
#include "macros.h"

int F90(test) (int a) {
    return a * 10;
}

int F90(write) (int fd, const void *buf, int count) {
    return write(fd, buf, count);
}