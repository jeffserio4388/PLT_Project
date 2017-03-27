void hello() {
    sleep(3);
    printf("hello\n");
}
print("above\n");

pipe hello {
    hello();
}

pipe world {
    print("world\n");
}

print("below\n");

