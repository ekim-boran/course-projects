foldr =
  let foldr1 f b xs = if xs == [] 
                        then b 
                        else let h = head xs in 
                             let t = tail xs in 
                               f h (foldr1 f b t) 
  in 
     foldr1

,

add = 
  let add1 x y = x + y in 
      add1

,

myList =   
  [1, 2, 3, 4, 5]


