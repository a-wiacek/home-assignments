#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Names of functions are mangled (see runtime.ll)

void printInt(int n) {
    printf("%d\n", n);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

char *readString() {
    char *str = NULL;
    size_t size = 0;
    ssize_t len = getline(&str, &size, stdin);
    if (str[len - 1] == '\n')
        str[len - 1] = 0;
    return str;
}

int readInt() {
    int n;
    scanf("%d", &n);
    readString(); // hack, "%d\n" doesn't work
    return n;
}

char *concatStrings(char *s, char *t) {
    size_t slen = strlen(s), tlen = strlen(t);
    char *u = malloc(slen + tlen + 1);
    if (!u) error();
    strcpy(u, s);
    strcpy(u + slen, t);
    return u;
}

bool compareStrings(char *s, char *t) {
    return strcmp(s, t) == 0;
}

void *_malloc(int elems, int size) {
    return calloc(elems, size);
}
