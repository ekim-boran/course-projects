#!/usr/bin/env bash

HOME_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
tests=(
    "trace01.txt" 
    "trace02.txt"
    "trace03.txt"
    "trace04.txt" 
    "trace05.txt" 
    "trace06.txt"
    "trace07.txt" 
    "trace08.txt" 
    "trace09.txt"
    "trace10.txt"   
    "trace11.txt"   
    "trace12.txt"
    "trace13.txt" 
    "trace14.txt"  
    "trace15.txt"
    "trace16.txt"
    )
score=0
point=5

echo "\nCS:APP Shell Lab: Grading Sheet for tsh.c\n\n"

#
# Compile the student's tsh.c file
#
echo "Compiling your shell"
echo ""
cd ${HOME_DIR}
make clean >/dev/null 2>&1 && make build || echo "ERROR: tsh.c did not compile."

#
# Run the autograder
#
echo "Correctness Tests"
echo ""
for test in ${tests[@]}; do
    timeout 25 ./checktsh.pl -e -t $test
    if [ $? -eq 0 ]; then
        score=$((score + point))
    fi
done

totalScore=$(($point * ${#tests[@]}))
echo "Score = $score/$totalScore"
