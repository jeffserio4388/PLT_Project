void world() {
    sleep(3);
    fprintf(stderr, "world\n");
}
printf("above\n");

pipe hello {
    hello();
}

pipe hello {
    fprintf(stderr, "hello ");
}

printf("below\n");

