import Prelude hiding (product, Just, Nothing, Maybe) -- prevents library function from conflicting with the current defined one

-- product - is a function that produces the product of a list of numbers
-- e.g product [1, 2, 3, 4, 5] = 120
product :: Num a => [a] -> a
product [] = 1 
product (n:ns) = n * product ns

-- lastElement - is a function that selects the last element of a
-- non-empty list
-- e.g. lastElement [10, 20, 30] = 30, lastElement [] = ""
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = ""
    show (Just a) = show a

lastElement :: [Int] -> Maybe Int
lastElement [] = Nothing
lastElement ns = Just (ns !! (length(ns) - 1))

-- halve - is a function that splits a list with an even length into two halves
halve :: [a] -> ([a], [a])
halve ns = (take (length(ns) `div` 2) ns, drop (length(ns) `div` 2) ns)

-- safetail - is a function similar to tail that maps the empty list to itself rather than producing an error
-- three different functions that returns the tail of a list
-- conditional expression
safetailC :: [a] -> [a]
safetailC ns = if null ns == True then [] else tail ns

-- guarded expression
safetailG :: [a] -> [a]
safetailG ns | null ns = []
             | otherwise = tail ns

-- pattern matching
safetailP :: [a] -> [a]
safetailP [] = []
safetailP ns = tail ns

-- This function evaluate an expression to its integer value, and
-- return the list of integet values contained in an expression
data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

-- eval - is a function that evaluates an expression to an integer value
eval :: Expr -> Int
eval (Val x) = x
eval (App Add y z) = eval y + eval z
eval (App Mul y z) =  eval y * eval z

-- values - is a funciton that returns the list of integer values contained in an expression
values :: Expr -> [Int]
values (Val x) = [x]
values (App _ y z) = values y ++ [] ++ values z

-- delete - is a function that deletes the first occurence of a value in a list
-- e.g. delete 2[1, 2, 3, 2] gives [1, 3, 2] as a result
delete :: Int -> [Int] -> [Int]
findPos :: Eq a => a -> [a] -> [Int]
findPos x xs = [i | (y, i) <- zip xs [0..], x == y]
delete n ns = take(head(findPos n ns)) ns ++ drop(head(findPos n ns) + 1) ns

-- perms - is a function that returns all permutations of a list given by re-orderings of its elements
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms ns = [res | i <- ns, j <- perms(delete i ns), res <- [[i] ++ j]]

-- split - is a function that returns all splits of a list into two 
-- non-empty parts that can be appended to give the original list 
split :: [Int] -> [([Int], [Int])]
split ns = [(head_split, tail_split) | x <- [1..(length(ns)-1)], head_split <- [take x ns], tail_split <- [drop x ns]]

-- This function returns all expressions for all the values in a list

-- show how to display the expressions
instance Show Expr where
    show (Val n) = show n
    show (App Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
    show (App Mul l r) = "(" ++ show l ++ "*" ++ show r ++ ")"

-- show how to display the operators
instance Show Op where
    show Add = "+"
    show Mul = "*"

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n] 
exprs ns = [res | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, res <- [App opp l r | opp <- [Add, Mul]]]

-- solve - is a function that uses the previously defined functions to solve the countdown problem
-- it returns all expressions whose list of values is a permutation of the given list
solve ::[Int] -> Int -> [Expr]
solve ns n =  [e | ns' <- perms ns, e <- exprs ns', eval e == n]