#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fileIO.h"
#define BUFSIZE 4096


static inline char *readln(void *fp){
    File *file_object = (File *) fp;
    if (!fgets(file_object->buf, BUFSIZE, file_object->fp)) {
        if (ferror(fp)) {
            perror("File Read Error");
            exit(1);
        }
    }
    return file_object->buf;
}

static inline void writestr(char *str, FILE *fp){
    fprintf(stderr, "in writestr");
    if(!(fputs(str, fp))){
        perror("File Read Error");
        exit(1);
    }
}

static inline char *read_verbatum(void *fp){
    fprintf(stderr, "in read_verbatum\n");
    File *file_object = (File *) fp;
    if(!(fread((void *)file_object->buf, BUFSIZE, 1, file_object->fp))) {
        fprintf(stderr, "in read_verbatum-if\n");
        if (!feof(file_object->fp)) {
            perror("File Read Error");
            exit(1);
        }
    }
    file_object->buf[strlen(file_object->buf)] = '\0';
    fprintf(stderr, "in read_verbatum-end: %s\n", file_object->buf);
    return file_object->buf;
}

static inline void close_file(void * fp){
    fprintf(stderr, "in close_file\n");
    File *file_object = (File *) fp;
    free(file_object->buf);
    fclose(file_object->fp);
    return;
}


void init_file_obj(File *file_object){
    file_object->buf = (char *) malloc(BUFSIZE);
    if(!file_object->buf){
        perror("Error: file initialization failed");
        exit(1);
    }
    file_object->readln = readln;
    file_object->read = read_verbatum;
    file_object->writestr = writestr;
    file_object->close = close_file;
}

/*
int main(int argc, char **argv){
    File write_test;
    init_file_obj(&write_test);
    write_test.fp = fopen("test_IO.txt", "rb");
//    FILE *file_object = fopen("fileIO.h", "r+");
    //File write_test = {
    //    .fp = fopen("test_IO.txt", "rb"),
     //   .buf = (char *) malloc(BUFSIZE),
     //   .readln = readln,
    //    .read = read_verbatum,
    //    .writestr = writestr,
    //    .close = close_file
   // };
    //write_test.writestr("this\n", write_test.fp);
//    char buf[256];
//    fgets(buf, BUFSIZE, write_test.fp);
//    write_test.readln(&write_test);
    
    char *a = write_test.read(&write_test); 
//    write_test.writestr("Hello World\n", write_test.fp);
    //printf("yeah:%c\n", a[0]);
    //fclose(file_object);
//    write_test.fp =file_object;
    write_test.close(&write_test);
    return 0;
}*/
