import System.IO
import Data.List
import Control.Monad
import System.Exit

data Board = Board [[Char]] deriving (Show)

main = do
    let board = Board [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]
    gameLoop board 'X'

gameLoop :: Board -> Char -> IO ()
gameLoop (Board info) turn = do
    let prev = if turn == 'X' then 'O' else 'X'
    if checkWin (Board info) prev 
        then do 
            putStrLn ("Player " ++ [prev] ++ " wins") 
            exitSuccess
        else 
            putStr ""
    putStrLn "Enter x coord" 
    x <- getLine
    putStrLn "Enter y coord" 
    y <- getLine
    -- Convert coords to numbers and rotate so it makes more sense 
    let xCoord = 4 - (read y :: Int)
    let yCoord = (read x :: Int)
    if not $ checkValid (Board info) xCoord yCoord 
        then do 
            putStrLn "Spot is already taken"
            gameLoop (Board info) turn 
        else do
            let next = if turn == 'X' then 'O' else 'X'
            printBoard $ replaceRow (Board info) xCoord yCoord (turn) 
            gameLoop (replaceRow (Board info) xCoord yCoord (turn)) next

-- Code for updating the board, replace row finds the right row and passes it to replace point
replaceRow :: Board -> Int -> Int -> Char -> Board
replaceRow (Board info) 1 y c = Board ([replacePoint (info !! 0) y c] ++ [info !! 1] ++ [info !! 2])
replaceRow (Board info) 2 y c = Board ([info !! 0] ++ [replacePoint (info !! 1) y c] ++ [info !! 2])
replaceRow (Board info) 3 y c = Board ([info !! 0] ++ [info !! 1] ++ [replacePoint (info !! 2) y c])
    
replacePoint :: [Char] -> Int -> Char -> [Char]
replacePoint row 1 c = [c] ++ [row !! 1] ++ [row !! 2]
replacePoint row 2 c = [row !! 0] ++ [c] ++ [row !! 2]
replacePoint row 3 c = [row !! 0] ++ [row !! 1] ++ [c]

-- Checks all possible win conditions using checkrows, checkcols, and checkdiags
checkWin :: Board -> Char -> Bool
checkWin board player = checkRows board player || checkCols board player || checkDiag board player

checkRows :: Board -> Char -> Bool
checkRows (Board info) player = 
    case info of
        [[a, b, c], [d, e, f], [g, h, i]] ->
            a == player && a == b && b == c ||
            d == player && d == e && e == f || 
            g == player && g == h && h == i

checkCols :: Board -> Char -> Bool
checkCols (Board info) player = 
    case info of 
        [[a, d, g], [b, e, h], [c, f, i]] -> 
            (a == player && a == b && b == c) || 
            (d == player && d == e && e == f) || 
            (g == player && g == h && h == i)

checkDiag :: Board -> Char -> Bool
checkDiag (Board info) player = 
    case info of
        [[a, _, d], [_, b, _], [e, _, c]] -> 
            (a == player && a == b && b == c) || 
            (d == player && d == b && b == e)

-- Checks if a point is empty or not
checkValid :: Board -> Int -> Int -> Bool
checkValid (Board info) x y = (info !! (x-1)) !! (y-1) == ' '


printBoard :: Board -> IO ()
printBoard (Board info) = do
    let l1 = info !! 0
    let l2 = info !! 1
    let l3 = info !! 2
    putStr $ unlines [l1, l2, l3]

