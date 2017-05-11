struct test{
    int a;
    string b;
};

struct test2{
    int a;
    float b;
};
{
struct test x;
struct test2 y;
x.a = 1;
y.a = 10;
print_int(x.a);
print_int(y.a);
}
