findLast :: [a] -> a
findLast [] = error "No such thing"
findLast [x] = x
findLast (_:xs) = findLast xs

beforeLast :: [a] -> a
beforeLast [] = error "Empty list"
beforeLast [x] = error "Only one in list"
beforeLast [x,y] = x
beforeLast (x:y:rest) = beforeLast (y:rest)

elementAt :: [a] -> Int -> a
elementAt [] _  = error "Empty list"
elementAt _ n | n < 0  = error "No number"
elementAt (x:_) 0 = x
elementAt (_:xs) n = elementAt xs (n - 1)
