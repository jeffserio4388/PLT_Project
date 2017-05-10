{
int i;
for (i = 99; i >0; i = i-1)
    {       

    string wall = " bottles of beer on the wall";
    string beer = " bottles of beer.";
    string pass = "Take one down and paas it around, ";
    string nomore = "No more bottle of beer on the wall";
    if(i>1){
        print_int(i);
        print_str(wall$", ");
		print_int(i);
		print_str(beer$"\n");
        print_str(pass);
        print_int(i-1);
        print_str(wall$".\n\n");
        }
    else{
        print_int(i);
        print_str(wall$",");
        print_int(i);
        print_str(beer$"\n");
        print_str(pass);
        print_str(nomore$"\n\n");
        print_str(nomore $ ", no more bottle of beer"$"\n");
        print_str("Go to the store and buy some more, 99 bottles of beer on the wall.\n");
        }
    }
    }
