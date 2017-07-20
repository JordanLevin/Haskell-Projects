import System.IO
import Data.List
import Control.Monad

{-data Board = Board Char Char Char Char Char Char Char Char Char-}
{-data Board = Board [[Char]]-}
data Board = Board [[Char]] deriving (Show)

{-main = do-}
    {-let board = Board ((' ',' ',' '),(' ',' ',' '),(' ',' ',' '))-}
    {-let board = Board [' ',' ',' ',' ',' ',' ',' ',' ',' ']-}
    {-let board = Board ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '-}
    {-turn board-}
main = do
    let board = Board [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]
    gameLoop board 'X'

gameLoop :: Board -> Char -> IO ()
gameLoop (Board info) turn = do
    {-hSetBuffering stdin NoBuffering-}
    putStrLn "Enter x coord" 
    x <- getLine
    putStrLn "Enter y coord" 
    y <- getLine
    let next = if turn == 'X' then 'O' else 'X'
    printBoard $ replaceRow (Board info) (read x :: Int) (read y :: Int) (turn) 
    gameLoop (replaceRow (Board info) (read x :: Int) (read y :: Int) (turn)) next

replaceRow :: Board -> Int -> Int -> Char -> Board
replaceRow (Board info) 1 y c = Board ([replacePoint (info !! 0) y c] ++ [info !! 1] ++ [info !! 2])
replaceRow (Board info) 2 y c = Board ([info !! 0] ++ [replacePoint (info !! 1) y c] ++ [info !! 2])
replaceRow (Board info) 3 y c = Board ([info !! 0] ++ [info !! 1] ++ [replacePoint (info !! 2) y c])
    
replacePoint :: [Char] -> Int -> Char -> [Char]
replacePoint row 1 c = [c] ++ [row !! 1] ++ [row !! 2]
replacePoint row 2 c = [row !! 0] ++ [c] ++ [row !! 2]
replacePoint row 3 c = [row !! 0] ++ [row !! 1] ++ [c]


printBoard :: Board -> IO ()
printBoard (Board info) = do
    let l1 = info !! 0
    let l2 = info !! 1
    let l3 = info !! 2
    putStr $ unlines [l1, l2, l3]

