function hello() {
    sleep(3);
    print("hello\n");
}

function main() {
    print("above\n");

    pipe {
        hello();
    }

    pipe {
        print("world\n");
    }
    
    print("below\n");
}
