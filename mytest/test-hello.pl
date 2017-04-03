function void hello() {
    sleep(3);
    printf("hello\n");
}
printf("above\n");

pipe hello {
    hello();
}

pipe world {
    printf("world\n");
}

printf("below\n");

