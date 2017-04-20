/* struct as a type in another struct: should pass */

struct A{
    int i;
};

struct B{
    struct A a;
};
