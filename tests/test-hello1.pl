function void world() {
    sleep(3);
    print_str("world\n");
}

print_str("above\n");

pipe {
    world();
}

pipe {
    print_str("hello ");
}

print_str("below\n");

