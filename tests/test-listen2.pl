pipe server {
    listen("0.0.0.0", 8080);
    printf("Hi, i'm on port 8080!");
}

pipe server2 {
    listen("0.0.0.0", 8081);
    printf("Hi, i'm on port 8081!");
}
