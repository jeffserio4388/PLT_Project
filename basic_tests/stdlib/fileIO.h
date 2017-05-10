#ifndef __FILE_IO_H__
#define __FILE_IO_H__

#include <stdio.h>
#include <stdlib.h>

typedef struct File{
    FILE *fp;
    char *buf;
    char *(*readln)(void *fp);
    void (*writestr)(char *str, FILE *fp);
    char *(*readn)(void *fp, int n);
    void (*close)(void *fp);
} File;

void init_file_obj(File *file_object);
#endif

