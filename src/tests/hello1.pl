void world() {
    sleep(3);
    fprintf(stderr, "world\n");
}
printf("above\n");

pipe world {
    world);
}

pipe hello {
    fprintf(stderr, "hello ");
}

printf("below\n");

