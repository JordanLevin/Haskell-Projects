data Tree a = Leaf a | Leaf a Tree a Tree a

-- Insert second value into the first tree and return a new tree
insert :: (Ord a) => Tree -> a -> Tree
insert (Leaf x) val = Leaf val
insert (Tree leaf t1 t2) = if val < leaf then insert t1 val else insert t2 val
