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
safetialG ns | null ns = []
			 | otherwise = tail ns

-- pattern matching
safetailP :: [a] -> [a]
safetailP [] = []
safetialP ns = tail ns

