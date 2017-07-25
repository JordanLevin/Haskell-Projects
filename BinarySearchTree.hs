data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Insert second value into the first tree and return a new tree
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty val = Node val Empty Empty
insert (Node val t1 t2) input
    | input < val = Node val t1 (insert t2 input) 
    | input > val = Node val (insert t1 input) t2
    | input == val = error "Cannot have duplicates in a bst"

--Infix synonym for insert for easier typing
(.+) :: (Ord a) => Tree a -> a -> Tree a
tree .+ val = insert tree val

--Returns the size of a tree
size :: Tree a -> Int
size Empty = 0
size (Node _ Empty Empty) = 1
size (Node _ t1 t2) = 1 + size t1 + size t2

--Returns a list containing all values traversed in order (I think)
traverseBST :: Tree a -> [a]
traverseBST Empty = []
traverseBST (Node val t1 t2) = traverseBST t2 ++ val : traverseBST t1

--Returns a boolean based on if a value is in the tree
find :: (Ord a) => Tree a -> a -> Bool
find Empty _ = False
find (Node val t1 t2) target
    | target == val = True
    | target > val = find t1 target 
    | target < val = find t2 target 
--works up to here

--Find a certain value and return a new BST without that value in it
--Remove handles root, if that doesn't happen it passed to remove helper
remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty _ = Empty
remove (Node val t1 t2) target
    | val == target = 

removeHelper :: (Ord a) => Tree a -> a -> Tree a
