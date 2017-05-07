struct A {
    int i;
    int k;
    bool j;
};
/* global bool a = true; 
int j;
j = 1;
*/
pipe { 
    int idk; 
    bool jk = true;
    /*string v = "td" + "sd"; */
}
global int g = 1;
pipe {
    int j = 1;
/*    bool i = true; */

}
function void foo(int x, int y) {
    File file_test;
    /*File file_testa = file_test;*/
    init_file_obj(file_test, "test_file.txt", "r+");
    close_file(file_test);
    int i = x + y;
    struct A a;
    struct A b = a;
    float j = 1.0 + 2.222;
    float ij = i + j;
    a.i = 1;
    string k;
    k = "abcd";
    string f = "abcd" $ k $ k;
    print_str(f);
/*    string ka = k + "1234"; */
    for( i = 0; i < 2; i = i + 1){
        int temp = 0;
        temp = temp + 1;
        print_int(temp);
        print_str(k);
        print_str("\n");
    }
}
int testa;
int testb = 1;
foo(1, 2);
/* g = a; */

