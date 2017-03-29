void world(){
    sleep(3);
    printf("World\n");
}
pipe world {
    world();
}

printf("Hello ");

