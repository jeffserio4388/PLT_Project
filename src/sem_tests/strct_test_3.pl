/* Recursive struct: should fail */
struct A{
    struct A a;
};
