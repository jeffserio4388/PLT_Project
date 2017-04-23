#include <uv.h>

#ifndef _MYLIST_H_
#define _MYLIST_H_


/*
 * A request object containing necessary info for routing
 */
struct Request {
    char *data;
    void (*f)(void *);
    
};


#endif /* #ifndef _MYLIST_H_ */
