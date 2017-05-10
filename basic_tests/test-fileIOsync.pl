{
File test_file;
init_file_obj(test_file, "test-fileIO.txt", "w+");
fwrite_str("Hello World\n", test_file);
close_file(test_file);
init_file_obj(test_file, "test-fileIO.txt", "r+");
string s = fread_line(test_file);
print_str(s);
close_file(test_file);
init_file_obj(test_file, "test-fileIO.txt", "r+");
s = freadn(test_file, 4);
print_str(s);
print_str("\n");
close_file(test_file);
}
