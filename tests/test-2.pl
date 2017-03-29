int foo(int a, int b) {
    int i;
    i = a + b;
    return i;
}


int k;
k = 1 + foo(2,3);
printf("%d\n", k);
int i;
int j;
for (i = 0; i < 3; i = 1 + i) {
    j = i + 1;
    printf("(%d, %d)\n", j, i);
} 
