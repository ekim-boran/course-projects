
for d in */ ; do
    cd "$d"
    python3 test.final
    cd ..
done
