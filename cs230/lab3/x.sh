 for i in {0..719} 
 do
    echo "$i" 
    python x.py ${i}  | ./bomb
done