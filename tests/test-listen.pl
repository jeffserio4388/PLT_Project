function void one() {}

function void two() {}

function void process() {}

pipe server {
    listen(1, 1);
    {
        process();
        process2()
    }
}
