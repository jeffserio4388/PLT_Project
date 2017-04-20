function void one() {}

function void two() {}

function void process() {}

pipe server {
    one();
    two();
    listen(1, 1);
    process();
}
