findLast :: [a] -> a
findLast [] = error "No such thing"
findLast [x] = x
findLast (_:xs) = findLast xs

beforeLast :: [a] -> a
beforeLast [] = error "Empty list"
beforeLast [x] = error "Only one in list"
beforeLast [x,y] = x
beforeLast (x:y:rest) = beforeLast (y:rest)


