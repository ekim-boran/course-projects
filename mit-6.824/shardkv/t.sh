A=4
B=10
rm -f ./test/*
for ((x=0;x<A;x++)); do
    for ((i=0;i<B;i++)); do
    a=$(( x * B + i ))
    time go test -v --race  >  test/$a.txt &
    pids[$i]=$!
    done
    for pid in ${pids[*]}; do
        wait $pid
    done
done

#    time go test -v -run TestChallenge2Unaffected --race  >  test/$a.txt &
