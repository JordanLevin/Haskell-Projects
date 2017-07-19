import System.IO
import Data.List
import Control.Monad

{-data Board = Board Char Char Char Char Char Char Char Char Char-}
{-data Board = Board [[Char]]-}
data Board = Board [Char] deriving (Show)

{-main = do-}
    {-let board = Board ((' ',' ',' '),(' ',' ',' '),(' ',' ',' '))-}
    {-let board = Board ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '-}
    {-let board = Board [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]-}
    {-turn board-}
main = do
    let board = Board [' ',' ',' ',' ',' ',' ',' ',' ',' ']
    gameLoop board

gameLoop :: Board -> IO ()
gameLoop (Board info) = forever $ do
    putStrLn "Enter a move (Eventually this will be coords)" 
    num <- getLine
    putStrLn "Enter a character (this is temporary)"
    val <- getChar
    let pos = (read num :: Int)
    let b = Board info
    printBoard $ turn b pos val

    

turn :: Board -> Int -> Char -> Board
turn (Board info) pos c = Board (take (pos-1) info ++ c:drop pos info)

printBoard :: Board -> IO ()
printBoard (Board info) = do
    let l1 = take 3 info
    let l2 = drop 3 $ take 6 info
    let l3 = drop 6 info
    putStr $ unlines [l1, l2, l3]

