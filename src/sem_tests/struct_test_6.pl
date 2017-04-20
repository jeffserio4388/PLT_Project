/* basic struct declaration 2: should pass */

struct A{
    int a;
    string b;
    bool c;
};

struct A a;
a.a = 1;
a.b = "hello world"
a.c = true;
printf(a.b);
