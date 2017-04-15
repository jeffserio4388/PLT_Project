files="../tests/test-*.pl"

make

for file in $files
do
    echo $file
    ./pipeline -d $file
    if []
    then
    else
    fi
done
