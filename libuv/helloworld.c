#include <stdio.h>
#include <uv.h>
#include <unistd.h>

int64_t counter = 0;

void hello() {
    sleep(3);
    printf("hello\n");
}

void idle_1(uv_idle_t* handle) {
    hello();
    uv_idle_stop(handle);
}

void idle_2(uv_idle_t* handle) {
    printf("world\n");
    uv_idle_stop(handle);
}

int main() {
    uv_idle_t idler;

    uv_idle_init(uv_default_loop(), &idler);
    uv_idle_start(&idler, idle_1);

    uv_idle_t idler_2;

    uv_idle_init(uv_default_loop(), &idler_2);
    uv_idle_start(&idler_2, idle_2);

    printf("above\n");
    printf("below\n");
    uv_run(uv_default_loop(), UV_RUN_DEFAULT);

    uv_loop_close(uv_default_loop());
    return 0;
}
