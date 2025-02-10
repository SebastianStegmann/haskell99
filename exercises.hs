findLast :: [a] -> a
findLast [] = error "No such thing"
findLast [x] = x
findLast (_:xs) = findLast xs

