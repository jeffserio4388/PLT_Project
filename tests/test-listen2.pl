pipe server {
    listen("127.0.0.1", 8080);
    printf("Hi, i'm on port 8080!");
}

pipe server2 {
    listen("127.0.0.1", 8081);
    printf("Hi, i'm on port 8081!");
}
