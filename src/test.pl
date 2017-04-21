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


