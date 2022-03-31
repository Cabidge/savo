#include <stdio.h>

void putfc(double n) {
    char ch = (char)n;
    putchar(ch);
}

double getfc() {
    char ch = getchar();
    return (double)ch;
}

void dumpf(double n) {
    printf("%.16g\n", n);
}