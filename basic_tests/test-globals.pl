global int a = 10;
global int b = 11;
function void foo(){
    int i = a + b;
    print_int(i);
    print_str("\n");
}
{
    foo();
}
