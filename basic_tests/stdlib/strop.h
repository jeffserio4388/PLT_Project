#ifndef _STROP_H_
#define _STROP_H_
#include<string.h>

/*
static char* _STR_BUF[4097];
char ** count = _STR_BUF;
*_STR_BUF = '\0';
*/
char* stringcat(const char* a, const char* b){
    char tmp[strlen(a)+strlen(b)];
    strcpy(tmp,a);
    strcat(tmp,b);
    return strdup(tmp);
    
}
/*
void Free_strs(){
    while(*_STR_BUF)
        free(*_STR_BUF++);
}
void foo(){
    Free_strs();
}
*/
#endif
