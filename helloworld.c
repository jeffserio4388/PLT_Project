#include <stdio.h>
#include <unistd.h>
#include <uv.h>
#include <stdlib.h>

void hello()
{
sleep(3);
printf("hello\n");
}

 void idle_world (uv_idle_t* handle) { 
print("world\n");
uv_idle_stop(handle);
}
void idle_hello (uv_idle_t* handle) { 
hello();
uv_idle_stop(handle);
}
int main(){
print("below\n");

print("above\n");

uv_idle_t idler_world;
uv_idle_init(uv_default_loop(), &idler_world);
uv_idle_start(&idler, idle_world);

uv_idle_t idler_hello;
uv_idle_init(uv_default_loop(), &idler_hello);
uv_idle_start(&idler, idle_hello);

uv_run(uv_default_loop(), UV_RUN_DEFAULT);
 uv_loop_close(uv_default_loop());
return 0; }
