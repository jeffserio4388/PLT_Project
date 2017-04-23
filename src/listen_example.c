#include <stdio.h>
#include <unistd.h>
#include <uv.h>
#include <stdlib.h>
#include "stdlib/mylist.h"

#define DEFAULT_PORT 7000
#define DEFAULT_BACKLOG 128

uv_loop_t *loop;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char*) malloc(suggested_size);
    buf->len = suggested_size;
}

void process() {}

void two() {}

void one() {}

void after(uv_work_t *req, int status) { }


// _____________________________________


uv_tcp_t tcp_server;
struct sockaddr_in addr_server;
uv_work_t req_listen_server;

void post_listen_server(uv_work_t *req) {
    fprintf(stderr, "%s\n", req->data);
    process();
}

void onread_server(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        req_listen_server.data = (void *) buf->base;
        uv_queue_work(loop, &req_listen_server, post_listen_server, after);
        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            fprintf(stderr, "Read error %s\n", uv_err_name(nread));
        uv_close((uv_handle_t*) client, NULL);
    }

    // free(buf->base);
}

void on_new_connection_server(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, "New connection error %s\n", uv_strerror(status));
        // error!
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);
    if (uv_accept(server, (uv_stream_t*) client) == 0) {
        uv_read_start((uv_stream_t*) client, alloc_buffer, onread_server);
    }
    else {
        uv_close((uv_handle_t*) client, NULL);
    }
}


void listen_server(char *ip_addr, int port) {
    uv_tcp_init(loop, &tcp_server);

    uv_ip4_addr(ip_addr, port, &addr_server);

    uv_tcp_bind(&tcp_server, (const struct sockaddr*) &addr_server, 0);
    printf("before uvrun before uvlisten");
    int r = uv_listen((uv_stream_t*) &tcp_server, DEFAULT_BACKLOG, on_new_connection_server);
    if (r) {
        fprintf(stderr, "Listen error %s\n", uv_strerror(r));
    }
}

void work_server(uv_work_t *req) {    
    one();
    two();
    listen_server("127.0.0.1", 8080);
}

int main() {
    loop = uv_default_loop();
    
    int data_server;
    uv_work_t req_server;
    req_server.data = (void *) &data_server;
    uv_queue_work(loop, &req_server, work_server, after);

    return uv_run(loop, UV_RUN_DEFAULT);
}
