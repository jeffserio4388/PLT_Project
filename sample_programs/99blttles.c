#include <stdio.h>
#include <unistd.h>
#include <uv.h>
#include <stdlib.h>

        #include <string.h>
#include <string.h>
#include "../stdlib/mylist.h"
#include "../stdlib/strop.h"
#define DEFAULT_PORT 7000
    #define DEFAULT_BACKLOG 128
    uv_loop_t *loop;
    int TEMP_FOR_ADD_LEFT;
    int TEMP_FOR_ADD_RIGHT;


int i_cast(void* data){
    return *(int * )data;
}

float f_cast(void *data){
    return *(float * )data;
}

char * c_cast(void* data){
    return *(char ** )data;
}

char *header =
"HTTP/1.0 200 OK\n"
"Date: Fri, 31 Dec 1999 23:59:59 GMT\n"
"\n";

char *makeHTTP(char *body) {
    int headerLength = strlen(header);
        int bodyLength = strlen(body);
    char *httpResult = (char* ) malloc(headerLength + bodyLength + 1);
    strcpy(httpResult, header);
    strcat(httpResult, body);

    return httpResult;
}


struct Backpack {
    uv_stream_t *client;
    char *data;
};

typedef struct {
    uv_write_t req;
    uv_buf_t buf;
} write_req_t;

void echo_write(uv_write_t *req, int status) {
    fprintf(stderr, "I did print\n");
    uv_close((uv_handle_t * ) req->data, NULL);
    if (status) {
        fprintf(stderr, "Write error %s\n", uv_strerror(status));
    }
    // free_write_req(req);
}


uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char * ) malloc(suggested_size);
    buf->len = suggested_size;
}

void after(uv_work_t *req, int status) { }



int main(int argc, char **argv) {
        loop = uv_default_loop();
{
int i;
for (i = 99 ; i>0 ; i = i-1) {
char * wall = " bottles of beer on the wall";
char * beer = " bottles of beer.";
char * pass = "Take one down and paas it around, ";
char * nomore = "No more bottle of beer on the wall";
if (i>1)
{
fprintf(stdout, "%d",i); fflush(stdout);
fprintf(stdout, "%s",stringcat(wall,", ")); fflush(stdout);
fprintf(stdout, "%d",i); fflush(stdout);
fprintf(stdout, "%s",stringcat(beer,"\n")); fflush(stdout);
fprintf(stdout, "%s",pass); fflush(stdout);
fprintf(stdout, "%d",i-1); fflush(stdout);
fprintf(stdout, "%s",stringcat(wall,".\n\n")); fflush(stdout);
}
else
{
fprintf(stdout, "%d",i); fflush(stdout);
fprintf(stdout, "%s",stringcat(wall,",")); fflush(stdout);
fprintf(stdout, "%d",i); fflush(stdout);
fprintf(stdout, "%s",stringcat(beer,"\n")); fflush(stdout);
fprintf(stdout, "%s",pass); fflush(stdout);
fprintf(stdout, "%s",stringcat(nomore,"\n\n")); fflush(stdout);
fprintf(stdout, "%s",stringcat(stringcat(nomore,", no more bottle of beer"),"\n")); fflush(stdout);
fprintf(stdout, "%s","Go to the store and buy some more, 99 bottles of beer on the wall.\n"); fflush(stdout);
}
}
}


    Free_strs();
    return uv_run(loop, UV_RUN_DEFAULT);
}
