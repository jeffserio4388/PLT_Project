#ifndef _STROP_H_
#define _STROP_H_
#include<string.h>

static char *STR_BUF[4097];
static int count = 0;

void Free_strs(){
    char **p = (char **) STR_BUF;
    while(*p)
        free(*p++);
}

char* stringcat(const char* a, const char* b){
    char tmp[strlen(a)+strlen(b)];
    strcpy(tmp,a);
    strcat(tmp,b);
    char * ptr = strdup(tmp);
    char **p = (char **) STR_BUF;
    if( count > 4096 ) {
        Free_strs();
        count = 0;
    }
    p[count++] = ptr;
    p[count] = NULL;
    return ptr;
    
}

#endif
