
for d in */ ; do
    cd "$d"
    make test
    rm -rf ./.stack-work
    make clean
    cd ..
done
