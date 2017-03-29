function foo( int a, int b) int {
    int i;
    i = a + b;
    return i;
}

function main() int {
    int k;
    k = 1 + foo(2,3);
    printf("%d\n", k);
    int i;
    for i = 0; i < 3; i = 1 + i {
        int j = i + 1
        printf("(%d, %d)\n", j, i) 
    return 0;
}
