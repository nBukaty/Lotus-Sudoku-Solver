import Data.List
import Data.Char
import Data.Maybe

{-|Lotus sudoku puzzle created by:
   Nick Bukaty and Jack Stevens
   EECS 368 Project 2
-}

{-|To solve the puzzle we find the first zero then generate a list of possible candidates
   each of those candidates is individually plugged into the array and lotusSolver is called on the new array
   when we reach a point with no candidates or run out of candidates for a given spot the function goes up a level
   The resursion relies mainly on the function "tryValues" 
-}
main = putStrLn $ show (lotusSolver [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0])

--a 2D list storing appropriate values to check the clockwise curl
curlClockwise = [[0,7,15,22,30,37,45],
                 [1,8,16,23,31,38,46],
                 [2,9,17,24,32,39,47],
                 [3,10,18,25,33,40,48],
                 [4,11,19,26,34,41,42],
                 [5,12,20,27,28,35,43],
                 [6,13,14,21,29,36,44]]::[[Int]]

--a 2D list storing appropriate values to check the counter clockwise curl
curlCntrclockwise = [[0,13,20,26,33,39,46],
                     [1,7,14,27,34,40,47],
                     [2,8,15,21,28,41,48],
                     [3,9,16,22,29,35,42],
                     [4,10,17,23,30,36,43],
                     [5,11,18,24,31,37,44],
                     [6,12,19,25,32,38,45]]::[[Int]]

--a 2D list storing appropriate values to check the rings
circleIndex = [[0,1,2,3,4,5,6],
               [7,8,9,10,11,12,13],
               [14,15,16,17,18,19,20],
               [21,22,23,24,25,26,27],
               [28,29,30,31,32,33,34],
               [35,36,37,38,39,40,41],
               [42,43,44,45,46,47,48]]::[[Int]]

--returns the index of the list in the clockwise curl that contains the given indexes for checking values from the Lotus
findClockwise::Int -> Int
findClockwise tryIndex
   | (tryIndex == 0)||(tryIndex == 7)||(tryIndex == 15)||(tryIndex == 22)||(tryIndex == 30)||(tryIndex == 37)||(tryIndex == 45) = 0
   | (tryIndex == 1)||(tryIndex == 8)||(tryIndex == 16)||(tryIndex == 23)||(tryIndex == 31)||(tryIndex == 38)||(tryIndex == 46) = 1
   | (tryIndex == 2)||(tryIndex == 9)||(tryIndex == 17)||(tryIndex == 24)||(tryIndex == 32)||(tryIndex == 39)||(tryIndex == 47) = 2
   | (tryIndex == 3)||(tryIndex == 10)||(tryIndex == 18)||(tryIndex == 25)||(tryIndex == 33)||(tryIndex == 40)||(tryIndex == 48) = 3
   | (tryIndex == 4)||(tryIndex == 11)||(tryIndex == 19)||(tryIndex == 26)||(tryIndex == 34)||(tryIndex == 41)||(tryIndex == 42) = 4
   | (tryIndex == 5)||(tryIndex == 12)||(tryIndex == 20)||(tryIndex == 27)||(tryIndex == 28)||(tryIndex == 35)||(tryIndex == 43) = 5
   | (tryIndex == 6)||(tryIndex == 13)||(tryIndex == 14)||(tryIndex == 21)||(tryIndex == 29)||(tryIndex == 36)||(tryIndex == 44) = 6
   | otherwise = 0

--returns the index of the list in the counter clockwise curl that contains the given indexes for checking values from the Lotus
findCntrClockwise::Int -> Int
findCntrClockwise tryIndex
   | (tryIndex == 0)||(tryIndex == 13)||(tryIndex == 20)||(tryIndex == 26)||(tryIndex == 33)||(tryIndex == 39)||(tryIndex == 46) = 0
   | (tryIndex == 1)||(tryIndex == 7)||(tryIndex == 14)||(tryIndex == 27)||(tryIndex == 34)||(tryIndex == 40)||(tryIndex == 47) = 1
   | (tryIndex == 2)||(tryIndex == 8)||(tryIndex == 15)||(tryIndex == 21)||(tryIndex == 28)||(tryIndex == 41)||(tryIndex == 48) = 2
   | (tryIndex == 3)||(tryIndex == 9)||(tryIndex == 16)||(tryIndex == 22)||(tryIndex == 29)||(tryIndex == 35)||(tryIndex == 42) = 3
   | (tryIndex == 4)||(tryIndex == 10)||(tryIndex == 17)||(tryIndex == 23)||(tryIndex == 30)||(tryIndex == 36)||(tryIndex == 43) = 4
   | (tryIndex == 5)||(tryIndex == 11)||(tryIndex == 18)||(tryIndex == 24)||(tryIndex == 31)||(tryIndex == 37)||(tryIndex == 44) = 5
   | (tryIndex == 6)||(tryIndex == 12)||(tryIndex == 19)||(tryIndex == 25)||(tryIndex == 32)||(tryIndex == 38)||(tryIndex == 45) = 6
   | otherwise = 0

--returns the index of the list in the circle that contains the given indexes for checking values from the Lotus
findCircle:: Int -> Int
findCircle tryIndex 
   | (tryIndex == 0)||(tryIndex == 1)||(tryIndex == 2)||(tryIndex == 3)||(tryIndex == 4)||(tryIndex == 5)||(tryIndex == 6) = 0
   | (tryIndex == 7)||(tryIndex == 8)||(tryIndex == 9)||(tryIndex == 10)||(tryIndex == 11)||(tryIndex == 12)||(tryIndex == 13) = 1
   | (tryIndex == 14)||(tryIndex == 15)||(tryIndex == 16)||(tryIndex == 17)||(tryIndex == 18)||(tryIndex == 19)||(tryIndex == 20) = 2
   | (tryIndex == 21)||(tryIndex == 22)||(tryIndex == 23)||(tryIndex == 24)||(tryIndex == 25)||(tryIndex == 26)||(tryIndex == 27) = 3
   | (tryIndex == 28)||(tryIndex == 29)||(tryIndex == 30)||(tryIndex == 31)||(tryIndex == 32)||(tryIndex == 33)||(tryIndex == 34) = 4
   | (tryIndex == 35)||(tryIndex == 36)||(tryIndex == 37)||(tryIndex == 38)||(tryIndex == 39)||(tryIndex == 40)||(tryIndex == 41) = 5
   | (tryIndex == 42)||(tryIndex == 43)||(tryIndex == 44)||(tryIndex == 45)||(tryIndex == 46)||(tryIndex == 47)||(tryIndex == 48) = 6
   | otherwise = 0

--checks if the puzzle is solved, if not it picks an index to solve gets a list of candidates and calls tryValues with those parameters
--returns the solved Lotus or an empty array up the recursive loop
lotusSolver:: [Int] -> [Int]
lotusSolver unsolved = if myIndex == Nothing
                       then unsolved
                       else tryValues unsolved tryList (fromJust myIndex)
   where myIndex = elemIndex 0 unsolved
         tryList = candidates unsolved (fromJust myIndex) 1

--input the array the index and "1" returns a list of possible candidates at index "a"
candidates:: [Int]-> Int -> Int -> [Int]
candidates xs a ind = 
                  if (ind < 8) --the long line checks which numbers work at this index "a"
                  then if (ind < 8)&&((xs !! ((curlClockwise !! index2) !! 0) /= ind)&&(xs !! ((curlClockwise !! index2) !! 1) /= ind)&&(xs !! ((curlClockwise !! index2) !! 2) /= ind)&&(xs !! ((curlClockwise !! index2) !! 3) /= ind)&&(xs !! ((curlClockwise !! index2) !! 4) /= ind)&&(xs !! ((curlClockwise !! index2) !! 5) /= ind)&&(xs !! ((curlClockwise !! index2) !! 6) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 0) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 1) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 2) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 3) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 4) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 5) /= ind)&&(xs !! ((curlCntrclockwise !! index3) !! 6) /= ind)&&(xs !! ((circleIndex !! index4) !! 0) /= ind)&&(xs !! ((circleIndex !! index4) !! 1) /= ind)&&(xs !! ((circleIndex !! index4) !! 2) /= ind)&&(xs !! ((circleIndex !! index4) !! 3) /= ind)&&(xs !! ((circleIndex !! index4) !! 4) /= ind)&&(xs !! ((circleIndex !! index4) !! 5) /= ind)&&(xs !! ((circleIndex !! index4) !! 6) /= ind))
                       then candidates xs a (ind + 1) ++ [ind] -- recursively is called and if the if passes it adds which number works
                       else candidates xs a (ind + 1)
                  else [] --ends the recursion at value 8
   where
         index2 = findClockwise a -- these two variables are used to determine which of the 7 arcs need to be checked
         index3 = findCntrClockwise a
         index4 = findCircle a -- this variable is for determining which ring needs to be solved

--takes in the unsolved list and the possible candidates at a given position then recursively tries values one by one
tryValues:: [Int] -> [Int] -> Int -> [Int]
tryValues _ [] _ = [] --when the list of candidates is empty return an empty list that ultimately makes it back here
tryValues unsolved (x:xs) n = if solution == [] --through recursion the line above will return to this statement
                              then tryValues unsolved xs n --try the next value in the candidates list
                              else solution --the puzzle is solved
   where solution = solve unsolved n x

--plug in a given value at a given index and call lotus solver on the new list
--returns the solved puzzle or an empty array back to tryValues
solve:: [Int] -> Int -> Int -> [Int]
solve xs n try = if elemIndex 0 xs == Nothing --If the puzzle is solved then return the solved puzzle
                 then xs 
                 else lotusSolver (let (ys,_:zs) = splitAt n xs in ys ++ [try] ++ zs) --replace the number at index n with a value from candidates and call lotusSolver on the new list