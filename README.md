# PLT_Project
Repository for PLT team project codename: Pipeline

## How to Install and Run Pipeline

## Step 1: Libuv

Download libuv from the following link: http://dist.libuv.org/dist/v1.11.0/
Follow the instructions to install:
    $ sh autogen.sh
    $ ./configure
    $ make
    $ make check
    $ make install

Add the following line to your bashrc:
    export LD_LIBRARY_PATH="/usr/local/lib"

## Step 2: Pipeline
    navigate to the src folder
    enter command 'make'

## Step 3: Run files with the pipeline executable as follows:
    ./pipeline -<flag> <filename.pl>
    where <flag> can be any of the following
        ("-a", Ast);        (* Print the AST *)
        ("-t", Translate);  (* Translate the program to output c file *)
        ("-c", Compile);    (* Compile the output c file *)
        ("-r", Run);        (* Run the program *)
        ("-d", Compare) ]   (* Compare output and expected output *)




