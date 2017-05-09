function string get_animals() {
    return "I love to eat them";
}

function string get_dogs() {
    return "I love to pet them";
}

function string get_pizza() {
    return "Domino's carry out special is the bomb diggity";
}

function string get_beer() {
    return "The best bro";
}

function string get_school() {
    return "School sucks";
}

pipe {
    listen("127.0.0.1", 8080);
    http("GET", "/animals", "get_animals");
    http("GET", "/dogs", "get_dogs");
    http("GET", "/pizza", "get_pizza");
    http("GET", "/beer", "get_beer");
    http("GET", "/school", "get_school");
}
