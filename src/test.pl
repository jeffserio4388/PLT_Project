pipe {
    struct Server server = listen("127.0.0.1", 8080);
    http_get(server, "/user", func);
    http_put(server, "/user", func1);
    http_get(server, "/user", func);
    http_get(server, "/user", func);
    http_get(server, "/user", func);
    
}

























pipe {
    f = process(a);
    g = sendResultToUser(b);
    // a = listen(80);
    b = process(a);
    c = sendResultToUser(b);
}

uv_listen
callback {
    take in loop

    uv_queue_work(func);
}

create a function like this func() {
    b = process(a);
    c = sendResultToUser(b);
}


