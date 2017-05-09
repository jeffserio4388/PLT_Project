function string get_user_handler(){
    return "You sent me a GET request !!!???! ";
}
function string put_user_handler(){
    return "You sent me a PUT request !!!???! ";
}
function string post_user_handler(){
    return "You sent me a POST request !!!???! ";
}
function string delete_user_handler(){
    return "You sent me a DELETE request !!!???! ";
}

pipe {
    listen("127.0.0.1",8080);
    http("GET","/user","get_user_handler");
    http("PUT","/user","put_user_handler");
    http("POST","/user","post_user_handler");
    http("DELETE","/user","delete_user_handler");

}



