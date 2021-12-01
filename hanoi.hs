import Data.Char

data Board = Tower [Int] [Int] [Int] deriving (Eq, Show)

main :: IO ()
main = do 
    goto 1 1
    clearAll
    putStr "Start a new game with: b <numOfRings> or quit with 'q'"
    newline
    input <- getLine 
   
    case words input of 
        ("b":n:_) | isNat n -> do startGame n
                
        ("q":_) -> do return ()
        _ -> do 
            main

startGame :: [Char] -> IO()
startGame n = play [new]
    where 
        new = newGame (read' n)

play :: [Board] -> IO()
play (hanoi:hanois) = do 
    if(done hanoi) then do 
        congrats (hanoi:hanois) 
    else do 
    printBoard hanoi 
    newline
    putStrLn ("Number of moves: " ++ show (length (hanoi:hanois)-1))
    input <- getLine 
    
    case words input of 
        ("b":n:_) | isNat n -> do startGame n
        (from:to:_) | isNat from && isNat to -> do
            if(inRange (read' from) && inRange (read' to)) then 
                if(validMove (tower hanoi (read' from)) (tower hanoi (read' to))) then do
                    play (move hanoi (read' from) (read' to):hanoi:hanois)
                    else do 
                        play (hanoi:hanois) 
            else do 
                play (hanoi:hanois)  
        ("z":undo:_) | isNat undo -> do
            play (goBack (hanoi:hanois) (read' undo))
        ["q"] -> return ()
        _ -> do 
            play (hanoi:hanois) 

---- Initial board ---------------------------------------------------

newGame :: Int -> Board
newGame n = 
    (Tower [1..n] [] []) 

---- From String to Int ----------------------------------------------

read' :: String -> Int 
read' x = read x::Int

---- Find number of disk ---------------------------------------------

numDisk :: Board -> Int 
numDisk (Tower a b c) = maximum list
    where list = a ++ b ++ c

---- Return specified tower ------------------------------------------

tower :: Board -> Int -> [Int]
tower (Tower a b c) n
    | n == 1 = a 
    | n == 2 = b 
    | otherwise = c
        
---- Change board ----------------------------------------------------

move :: Board -> Int -> Int -> Board 
move hanoi from to = (Tower (mv 1) (mv 2) (mv 3)) 
    where 
        mv :: Int -> [Int] 
        mv x =
            if(x == from) then tail (tower hanoi x)
            else if(x == to) then (head (tower hanoi from)):tower hanoi to
            else tower hanoi x
          
validMove :: [Int] -> [Int] -> Bool
validMove t1 t2 
    | null t1 = False 
    | null t2  = True
    | otherwise = elem_t1 < elem_t2
        where 
            elem_t1 = minimum t1 
            elem_t2 = minimum t2   

goBack :: [Board] -> Int -> [Board]
goBack hanoi undo = drop (min (undo) (length hanoi -1)) hanoi

---- Validator -------------------------------------------------------

inRange :: Int -> Bool
inRange x = x >= 1 && x <= 3

isNat :: String -> Bool
isNat "" = False
isNat s  = all isDigit s

done :: Board -> Bool 
done (Tower a b c) 
    | null a && null b = True 
    | otherwise = False 

---- Printing --------------------------------------------------------

newline :: IO ()
newline = putChar '\n'

tags :: Int -> String 
tags i = concat (replicate i "# ")

blank :: Int -> String 
blank i = concat (replicate i " ")

padList :: [Int] -> Int -> [Int]
padList xs n = replicate (n - (length xs) + 1) 0 ++ xs

padLine :: Int -> Int -> String 
padLine x max
    | x == 0 = blank (max-1) ++ "| " ++ blank (max-1)
    | otherwise = blank (max-x) ++ tags x ++ blank (max-x)

pad :: [Int] -> [Int] -> [Int] -> Int -> Int -> IO()
pad [] [] [] _ _ = return()
pad (x:xs) (y:ys) (z:zs) max n
    | n > max + 1 = return () 
    | otherwise = do 
        putStrLn (padLine x max ++ " " ++ padLine y max ++ " " ++ padLine z max)
        pad xs ys zs max(n+1) 

printBoard :: Board -> IO()
printBoard (Tower a b c) = do
    clearAll 
    goto 1 3
    pad t1 t2 t3 max 0
    where 
        list = a ++ b ++ c
        max = maximum list
        t1 = padList a max
        t2 = padList b max
        t3 = padList c max

congrats :: [Board]  -> IO()
congrats (xs:xss) = do 
    printBoard xs 
    newline 
    putStrLn ("Congratulation, you won!")
    putStrLn ("Number of moves: " ++ show (length (xs:xss)-1))
    putStrLn ("Number of disks: " ++ show (numDisk xs) )

----------------------------------------------------------------------

goto :: Int -> Int -> IO () 
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

clearAll :: IO() 
clearAll = putStr "\ESC[2J"

----------------------------------------------------------------------

