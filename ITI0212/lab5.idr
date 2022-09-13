import System.File
import Data.String

parseNat : String -> Maybe Nat
parseNat = parsePositive

fromMaybe : a -> Maybe a -> a
fromMaybe x (Just a) = a
fromMaybe x (Nothing) = x

getNat : IO Nat
getNat = do 
    putStrLn "Please Enter a Nat:"
    line <- getLine
    pure (fromMaybe 0 $ parseNat line)


insistNat : IO Nat
insistNat = do
  putStr "Please Enter a Nat: "
  line <- getLine
  case (parseNat line) of
       Nothing => insistNat
       Just n  => pure n

insistAdd : Nat -> IO Nat
insistAdd n = map (n+) insistNat

addAfter : (IO Nat) -> Nat -> IO Nat
addAfter io n = map (n+) io

natsGet : IO (Maybe (List Nat))
natsGet = map (traverse (parseNat) . (words  )) getLine  
   
testnatsGet : IO ()
testnatsGet = natsGet >>= ( putStrLn . show)



filterMaybes : List(Maybe a) -> List a
filterMaybes [] = []
filterMaybes (x::xs) = case x of
    Nothing => filterMaybes xs
    (Just a) => a :: filterMaybes xs

tryNats : IO (List Nat)
tryNats = map (filterMaybes .  map parseNat . words   ) getLine  

getLines : IO (List String)
getLines = do 
    putStrLn "enter line:"
    x <- getLine
    if x == "done" then pure [] else map (x::) getLines

dictate : IO ()
dictate = do
    lines <- getLines
    putStrLn  "Enter Storage Location"
    filename <- getLine
    if filename == "none" then putStrLn "throw away" else  
       do
       x <- writeFile filename (unlines lines) 
       case x of
            (Left e) => putStrLn "failed"
            (Right a) => putStrLn "success"

main : IO()
main = dictate