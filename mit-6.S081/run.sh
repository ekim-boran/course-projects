for s in 1 2 3 4 5 6 7 8 9 10 11
do
 	cd lab$s/xv6-labs-2020
	make grade
	make clean
 
	cd ..
	cd ..

done 
