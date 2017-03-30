void world(int seconds) {
    sleep(seconds);
    printf("world\n");
}

void hello() {
    printf("hello\n");
}

printf("above\n");

pipe hello {
    hello();
}

pipe world {
    world(3);
}



printf("below\n");
