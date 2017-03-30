void world() {
    printf("world\n");
}

void hello() {
    printf("hello\n");
}

printf("above\n");


pipe world {
    world();
}

pipe hello {
    hello();
}



printf("below\n");
