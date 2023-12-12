//"printInt", "printString", "error", "readInt", "readString", "concat"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void printInt(int v) {
    printf("%d\n", v);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    puts("runtime error");
    exit(1);
}

int readInt() {
    int v;
    scanf("%d", &v);
    return v;
}

char* readString() {
    char *line = NULL;
    size_t n = 0, read;
    read = getline(&line, &n, stdin);
    if (read == -1) {
        error();
    } 
    int nn = strlen(line);
    char *buf = malloc(nn);
    memmove(buf, line, nn);
    buf[nn - 1] = 0;
    return buf;
}

char *concat(char *a, char *b) {
    int na = strlen(a), nb = strlen(b);
    char *buf = malloc(1 + na + nb);
    memmove(buf, a, na);
    memmove(buf + na, b, nb);
    buf[na + nb] = 0;
    return buf;
}