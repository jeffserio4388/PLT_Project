#include <stdio.h>
#include <unistd.h>
#include <uv.h>
#include <stdlib.h>
#include "stdlib/mylist.h"
#include <string.h>
#define DEFAULT_PORT 7000
#define DEFAULT_BACKLOG 128


char *dat = 
"HTTP/1.0 200 OK\n"
"Date: Fri, 31 Dec 1999 23:59:59 GMT\n"
"Content-Type: text/html\n"
"Content-Length: 1354\n"
"\n"
"<html>\n"
"<body>\n"
"<h1>Happy New Millennium!</h1>\n"
"(more file contents)\n"
"</body>\n"
"</html>\n";

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


void after(uv_work_t *req, int status) { }

uv_tcp_t tcp_server2;
struct sockaddr_in addr_server2;
uv_work_t req_listen_server2;

void post_listen_server2(uv_work_t *req) {
    // fprintf(stderr, "%s", req->data);

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

    /*
    switch():
        function1(sfs)
        function2(sdfd)

        function1 (body
    */
	
	write_req_t *req_send = (write_req_t*) malloc(sizeof(write_req_t));
	req_send->buf = uv_buf_init(dat, strlen(dat));
	// fprintf(stderr, "buf base: %s\n", req->buf.base);
	// fprintf(stderr, "buf len: %d\n", req->buf.len);
	uv_write((uv_write_t*) req_send, ((struct Backpack*) (req->data))->client, &req_send->buf, 1, echo_write);


    int a;

    a = 5;

    a = 6;

    fprintf(stderr, "Hi, i'm on port 8081!\n");

    // free(req->data->data);
    // free(req->data);

}

void onread_server2(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {

        struct Backpack *backpack = (struct Backpack*) malloc(sizeof(struct Backpack));
        backpack->data = buf->base;
        backpack->client = client;

        req_listen_server2.data = (void *) backpack;
        uv_queue_work(loop, &req_listen_server2, post_listen_server2, after);
        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            fprintf(stderr, "Read error %s", uv_err_name(nread));
        uv_close((uv_handle_t*) client, NULL);
    }

}

void on_new_connection_server2(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, "New connection error %s", uv_strerror(status));
        // error!
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);
    if (uv_accept(server, (uv_stream_t*) client) == 0) {
        uv_read_start((uv_stream_t*) client, alloc_buffer, onread_server2);
    }
    else {
        uv_close((uv_handle_t*) client, NULL);
    }
}


void listen_server2(char *ip_addr, int port) {
    uv_tcp_init(loop, &tcp_server2);

    uv_ip4_addr(ip_addr, port, &addr_server2);

    uv_tcp_bind(&tcp_server2, (const struct sockaddr*) &addr_server2, 0);
    int r = uv_listen((uv_stream_t*) &tcp_server2, DEFAULT_BACKLOG, on_new_connection_server2);
    if (r) {
        fprintf(stderr, "Listen error %s", uv_strerror(r));
    }
}

void work_server2(uv_work_t *req) {    listen_server2("127.0.0.1", 8081);

}

uv_tcp_t tcp_server;
struct sockaddr_in addr_server;
uv_work_t req_listen_server;


void post_listen_server(uv_work_t *req) {
    fprintf(stderr, "%s", req->data);

    // put the switch statement here for method+route concatenated
    /*
    switch (case) {
    



    }
    */


    int a;

    a = 5;

    a = 6;

    fprintf(stderr, "Hi, i'm on port 8080!\n");

}

void onread_server(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        req_listen_server.data = (void *) buf->base;
        uv_queue_work(loop, &req_listen_server, post_listen_server, after);
        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            fprintf(stderr, "Read error %s", uv_err_name(nread));
        uv_close((uv_handle_t*) client, NULL);
    }

    // free(buf->base);
}

void on_new_connection_server(uv_stream_t *server, int status) {
    if (status < 0) {
        fprintf(stderr, "New connection error %s", uv_strerror(status));
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
    int r = uv_listen((uv_stream_t*) &tcp_server, DEFAULT_BACKLOG, on_new_connection_server);
    if (r) {
        fprintf(stderr, "Listen error %s", uv_strerror(r));
    }
}

void work_server(uv_work_t *req) {    listen_server("127.0.0.1", 8080);

}

int main() {
        loop = uv_default_loop();
    int data_server2;
    uv_work_t req_server2;
    req_server2.data = (void *) &data_server2;
    uv_queue_work(loop, &req_server2, work_server2, after);

    int data_server;
    uv_work_t req_server;
    req_server.data = (void *) &data_server;
    uv_queue_work(loop, &req_server, work_server, after);

    return uv_run(loop, UV_RUN_DEFAULT);
}
