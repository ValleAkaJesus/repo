{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
len:: [Int]->Int
len []=0
len (x:xs)=1+len(xs)


switch::Int->Int->[Int]->[Int]
switch a b xs = set a (get b xs) (set b (get a xs) xs)


set::Int->Int->[Int]->[Int]
set 0 val (x:xs) = val:xs
set n val (x:xs) = x:set (n-1) val xs


get::Int->[Int]->Int
get 0 (x:xs) = x
get n (x:xs) = get (n-1) xs
get _ [] = -1--um diese funktion auch als list.contains benutzen zu können

permutations::[Int]->[[Int]]
permutations []=[[]]
permutations [a,b] = [[a,b],[b,a]]
permutations (x:xs) = addToAll x (permutations (switch 0 1 xs))



addToAll::Int->[[Int]]->[[Int]]
addToAll _ []=[]
addToAll n (x:xs) = (n:x): addToAll n xs


pushToStart::Int->[Int]->[Int]
pushToStart 0 xs = xs
pushToStart a xs = pushToStart (a-1) (switch a (a-1) xs)


main :: IO ()
main = do 
    print (permutations [1,2,3])


{- 
pseudocode:
endList=[]

for x1 in 0,len(inputList):
    l1=switch 0, x1, inputList

    for x2 in 1,len(inputList):
        l2=switch 1, x2, l1

        for x3 in 2, len(inputList):
            l3=switch 2, x3, l2

            ... bis xn, n=len(inputList)

permWithSetFirst liste -> [...]
    if(i==l.length-1)return l
    
    case x:xs = permWithSetFirst(xs)

-}

