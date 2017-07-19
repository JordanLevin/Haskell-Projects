data Node a = Blank | Cons a (Node a) deriving (Show, Read, Eq, Ord)

getHead :: Node a -> a
getHead Blank = error "List is empty"
getHead (Cons val (_)) = val

getTail :: Node a -> a
getTail Blank = error "List is empty"
getTail (Cons val (Blank)) = val 
getTail (Cons val (next)) = getTail next

appendToList :: Node a -> a -> Node a
appendToList Blank x = Cons x Blank
appendToList l x = Cons x l
