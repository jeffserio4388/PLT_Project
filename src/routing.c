#include <stdio.h>
#include <unistd.h>
#include <uv.h>
#include <stdlib.h>
#include "stdlib/mylist.h"
#define DEFAULT_PORT 7000
#define DEFAULT_BACKLOG 128

char *header =
"HTTP/1.0 200 OK\n"
"Date: Fri, 31 Dec 1999 23:59:59 GMT\n"
"\n";

char *makeHTTP(char *body) {
    int headerLength = strlen(header);
        int bodyLength = strlen(body);
    char *httpResult = (char*) malloc(headerLength + bodyLength + 1);
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
    uv_close(((struct Backpack*) (req->data))->client, NULL);
    if (status) {
        fprintf(stderr, "Write error %s\n", uv_strerror(status));
    }
    // free_write_req(req);
}


uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char*) malloc(suggested_size);
    buf->len = suggested_size;
}

char * delete_user_handler() {
return "You sent me a DELETE request !!!???! ";
}


char * post_user_handler() {
return "You sent me a POST request !!!???! ";
}


char * put_user_handler() {
return "You sent me a PUT request !!!???! ";
}


char * get_user_handler() {
return "You sent me a GET request !!!???! ";
}


void after(uv_work_t *req, int status) { }

uv_tcp_t tcp_pipe_1;
struct sockaddr_in addr_pipe_1;
uv_work_t req_listen_pipe_1;
void post_listen_pipe_1(uv_work_t *req){
    char *token;
    char *method;
    char *route;
    char *protocol;

    token = strtok(((struct Backpack*) (req->data))->data, "\n");
    fprintf(stderr, "%s\n", token);

    method = strtok(token, " ");
    fprintf(stderr, "method: %s\n", method);

    route = strtok(NULL, " ");
    fprintf(stderr, "route: %s\n", route);

    protocol = strtok(NULL, " ");
    fprintf(stderr, "protocol: %s\n", protocol);

    int methodLength = strlen(method);
        int routeLength = strlen(route);
            char userVariable[routeLength + methodLength + 1];
                strcpy(userVariable, method);
                    strcat(userVariable, route);
                        fprintf(stderr, "userVariable: %s\n", userVariable); 
char* userResult = "";if (!strcmp("DELETE""/user", userVariable)) { userResult = delete_user_handler();} else
    if (!strcmp("POST""/user", userVariable)) { userResult = post_user_handler();} else
    if (!strcmp("PUT""/user", userVariable)) { userResult = put_user_handler();} else
    if (!strcmp("GET""/user", userVariable)) { userResult = get_user_handler();} else { fprintf(stderr,"no match"); } 
 
	write_req_t *req_send = (write_req_t*) malloc(sizeof(write_req_t));
    char *result = makeHTTP(userResult);
	req_send->buf = uv_buf_init(result, strlen(result));
    req_send->req.data = ((struct Backpack*) (req->data))->client;
	uv_write((uv_write_t*) req_send, ((struct Backpack*) (req->data))->client, &req_send->buf, 1, echo_write);
}


void onread_pipe_1(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        struct Backpack *backpack = (struct Backpack*) malloc(sizeof(struct Backpack));
        backpack->data = buf->base;
        backpack->client = client;

        req_listen_pipe_1.data = (void *) backpack;
        uv_queue_work(loop, &req_listen_pipe_1, post_listen_pipe_1, after);
        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            fprintf(stderr, "Read error %s", uv_err_name(nread));
        uv_close((uv_handle_t*) client, NULL);
    }

    // free(buf->base);
}

void on_new_connection_pipe_1(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, "New connection error %s", uv_strerror(status));
        // error!
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);
    if (uv_accept(server, (uv_stream_t*) client) == 0) {
        uv_read_start((uv_stream_t*) client, alloc_buffer, onread_pipe_1);
    }
    else {
        uv_close((uv_handle_t*) client, NULL);
    }
}




void listen_pipe_1(char *ip_addr, int port) {
    uv_tcp_init(loop, &tcp_pipe_1);

    uv_ip4_addr(ip_addr, port, &addr_pipe_1);

    uv_tcp_bind(&tcp_pipe_1, (const struct sockaddr*) &addr_pipe_1, 0);
    int r = uv_listen((uv_stream_t*) &tcp_pipe_1, DEFAULT_BACKLOG, on_new_connection_pipe_1);
    if (r) {
        fprintf(stderr, "Listen error %s", uv_strerror(r));
    }
}

void work_pipe_1(uv_work_t *req) {    listen_pipe_1("127.0.0.1", 8080);

}

int main() {
        loop = uv_default_loop();

    int data_pipe_1;
    uv_work_t req_pipe_1;
    req_pipe_1.data = (void *) &data_pipe_1;
    uv_queue_work(loop, &req_pipe_1, work_pipe_1, after);

    return uv_run(loop, UV_RUN_DEFAULT);
}
