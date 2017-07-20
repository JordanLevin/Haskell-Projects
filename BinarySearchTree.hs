data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Insert second value into the first tree and return a new tree
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty val = Node val Empty Empty
insert (Node val t1 t2) input
    | input < val = Node val (insert t1 input) t2
    | input > val = Node val (insert t2 input) t1
    | input == val = error "Cannot have duplicates in a bst"
