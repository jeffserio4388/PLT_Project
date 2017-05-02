global int g = 1;
/* global bool a = true; 
int j;
j = 1;
*/
pipe {
    int j = 1;
    bool i = true;

}
function void foo() {
    int i; 
    float j = 1.0 + 2.222;
    float ij = i + j;
    string k;
    k = "abcd";
    string ka = k + "1234";
    for( i = 0; i < 2; i = i + 1){
        int temp = 0;
        temp = temp + 1;
        print_int(temp);
        print_str(k);
        print_str("\n");
    }
}
string strng = "anbjdksf";
foo();
/* g = a; */

