pipe server {
    int a;
    a = 5;
    listen("127.0.0.1", 8080);
    a = 6;
}
