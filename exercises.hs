import Data.List (group)

-- 1
findLast :: [a] -> a
findLast [] = error "No such thing"
findLast [x] = x
findLast (_:xs) = findLast xs

-- 2
beforeLast :: [a] -> a
beforeLast [] = error "Empty list"
beforeLast [x] = error "Only one in list"
beforeLast [x,y] = x
beforeLast (x:y:rest) = beforeLast (y:rest)

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _  = error "Empty list"
elementAt _ n | n < 0  = error "No number"
elementAt (x:_) 0 = x
elementAt (_:xs) n = elementAt xs (n - 1)

-- 4
countElements :: [a] -> Int
countElements list = myElems list 0
  where
    myElems [] n = n
    myElems (_:xs) n = myElems xs (n + 1)

-- 5
reverseList :: [a] -> [a]
reverseList list = listAcc list []
  where
    listAcc [] revList = revList
    listAcc (x:xs) revList = listAcc xs revList ++ [x]
-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) =
  if x == findLast xs then
    isPalindrome (init xs)
  else
    False

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
 
compress :: Eq a => [a] -> [a]
compress = map head . group
-- tail, previous head, if head != old head add to ouput

-- 9
packDoublicates :: Eq a => [a] -> [[a]]
packDoublicates =  group
