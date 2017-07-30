import System.Random
    
data Suit = Spade | Heart | Club | Diamond deriving (Show)
data Card = Card Suit Int deriving (Show)
data Deck = Deck [Card] deriving (Show)

{-draw :: Deck -> Deck-}

makeDeck :: Deck
makeDeck = Deck $ makeDeckHelper 13 

makeDeckHelper :: Int -> [Card]
makeDeckHelper 1 = 
    [(Card Spade 1), (Card Heart 1) , (Card Club 1) , (Card Diamond 1)]
makeDeckHelper x = 
    [(Card Spade x), (Card Heart x) , (Card Club x) , (Card Diamond x)] ++ (makeDeckHelper $ x-1)

drawPos :: Deck -> Int -> (Deck, Card)
drawPos (Deck cards) pos
    | pos >= length cards = error "Invalid position in deck"
    | otherwise = (Deck $ (take pos cards) ++ (drop (pos+1) cards), cards !! pos)

addPos :: Deck -> Card -> Int -> Deck
addPos (Deck cards) card pos
    | pos > length cards = error "Invalid position in deck"
    | otherwise = Deck $ (take pos cards) ++ [card] ++ (drop (pos) cards)

addTop :: Deck -> Card -> Deck
addTop (Deck cards) card = addPos (Deck cards) card $ length cards

drawTop :: Deck -> (Deck, Card)
drawTop (Deck cards) = drawPos (Deck cards) $ length cards - 1

shuffleDeck :: Deck -> Deck
shuffleDeck (Deck cards) = 
