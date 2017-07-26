data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Insert second value into the first tree and return a new tree
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty val = Node val Empty Empty
insert (Node val t1 t2) input
    | input > val = Node val t1 (insert t2 input) 
    | input < val = Node val (insert t1 input) t2
    | input == val = error "Cannot have duplicates in a bst"

--Infix synonym for insert for easier typing
(.+) :: (Ord a) => Tree a -> a -> Tree a
tree .+ val = insert tree val

-- Returns the size of a tree
size :: Tree a -> Int
size Empty = 0
size (Node _ Empty Empty) = 1
size (Node _ t1 t2) = 1 + size t1 + size t2

-- Returns a list containing all values traversed in order (I think)
traverseBST :: Tree a -> [a]
traverseBST Empty = []
traverseBST (Node val t1 t2) = traverseBST t1 ++ val : traverseBST t2

--Returns a boolean based on if a value is in the tree
find :: (Ord a) => Tree a -> a -> Bool
find Empty _ = False
find (Node val t1 t2) target
    | target == val = True
    | target > val = find t2 target 
    | target < val = find t1 target 

-- Finds min val in a tree
findMin :: (Ord a) => Tree a -> a
findMin Empty = error "Empty tree has no min value"
findMin x = head $ traverseBST x

-- Finds max val in a tree
findMax :: (Ord a) => Tree a -> a
findMax Empty = error "Empty tree has no max value"
findMax x = last $ traverseBST x

-- Makes a tree with some ints for testing purposes
makeTestTree :: Tree Int
makeTestTree = do
    let a = insert Empty 10
    let b = a .+ 6
    let c = b .+ 2
    let d = c .+ 17
    let e = d .+ 12
    let f = e .+ 8
    f

-- Find a certain value and return a new BST without that value in it
remove :: (Ord a) => Tree a -> a -> Tree a
-- Tree is empty
remove Empty _ = Empty 
-- Tree is a leaf node
remove (Node val Empty Empty) target
    | target == val = Empty
    | otherwise = Node val Empty Empty
-- Tree only has 1 child branch
remove (Node val t1 Empty) target
    | target == val = t1
    | otherwise = Node val (remove t1 target) Empty
-- Tree only has 1 child branch
remove (Node val Empty t2) target
    | target == val = t2
    | otherwise = Node val Empty $ remove t2 target
-- Tree has two children
remove (Node val t1 t2) target
    | target == val = Node (t1Max) (remove t1 t1Max) t2
    | target < val = Node val (remove t1 target) t2
    | target > val = Node val t1 (remove t2 target)
    where t1Max = findMax t1
