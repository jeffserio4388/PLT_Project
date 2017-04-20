/* struct within a struct should fail */

struct A{
    int i;
    struct B{
        int j;
    };
};
