 
A=10
B=12
rm -f ./test/*
for ((x=0;x<A;x++)); do
    for ((i=0;i<B;i++)); do
    a=$(( x * B + i ))
    time go test   --race  >  test/$a.txt &
    pids[$i]=$!
    done
    for pid in ${pids[*]}; do
        wait $pid
    done
done