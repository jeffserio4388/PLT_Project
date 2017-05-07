#ifndef _STROP_H_
#define _STROP_H_
#include<string.h>

char* stringcat(const char* a, const char* b){
    char tmp[strlen(a)+strlen(b)];
    strcpy(tmp,a);
    strcat(tmp,b);
    return strdup(tmp);
}
#endif
