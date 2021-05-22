import Data.Array
    ( Array, elems, listArray, bounds, (!), (//), indices )


----------
-- Data
----------

-- | Represents possible cell states
data CellState = Alive | Zombie | Dead deriving (Eq,Show)

-- | Cell id
type Position = (Int,Int)

-- | Grid
type Grid = Array Position CellState

-- | Files names
fileName :: String
fileName = "game.txt"

-- | Var
outName :: String
outName = "out.txt"

--------------
-- Functions
--------------

-- | Parses an integer to a cell state.
fromIntegerC :: Int -> CellState
fromIntegerC 0 = Dead
fromIntegerC 1 = Alive
fromIntegerC 2 = Zombie

-- Parses a cell state to an integer.
toIntegerC :: CellState -> Int
toIntegerC Dead   = 0
toIntegerC Alive  = 1
toIntegerC Zombie = 2

-- Get index off the neighboors
getNeighboorhood :: Position -> Grid -> [CellState]
getNeighboorhood (row,col) grid = [grid ! p | p <- indexes]
    where
        -- Gets array bounds.
        ((minR,minC),(maxR,maxC))   = bounds grid
        -- Check the bounds
        inBoundsRow r = minR <= r && r <= maxR
        inBoundsCol c = minC <= c && c <= maxC
        -- Gets the valid index of the neighboors 
        indexes =   [ (r,c) | r <- [(row-1) .. (row+1)], c <- [(col-1) .. (col +1)] 
                    , inBoundsRow r && inBoundsCol c , (r,c) /= (row,col)]

-- | Given a Cell State and a list of Cell States, counts
-- how many elements of the list has the same state as the first argument. 
countCellState :: CellState -> [CellState] -> Int
countCellState state = length . filter (==state)

-- | Counts how many Dead Cells are in a list.
countDead :: [CellState] -> Int
countDead = countCellState Dead

-- | Counts how many Alive Cells are in a list.
countAlive :: [CellState] -> Int
countAlive = countCellState Alive

-- | Counts how many Zombie Cells are in a list.
countZombie :: [CellState] -> Int
countZombie = countCellState Zombie

-- Given a Cell, and a list of its neighboors, returns
-- the state of the cell in the next iteration.
nextState :: CellState -> [CellState] -> CellState
-- A Dead cell revives if it has exactly 3 alive neighboors, else it stays dead.
nextState Dead neighboors = if countAlive neighboors == 3 then  Alive else Dead
nextState Alive neighboors 
    -- If an alive cell has at least 1 zombie as a neighboor, it becomes a zombie.
    | countZombie neighboors >= 1 = Zombie
    -- If an alive cell has less than 2 alive neighboors, then it dies of under population.
    | countAlive neighboors < 2   = Dead
    -- if an alive cell has more than 3 alive neighboors, then it dies of over population.
    | countAlive neighboors > 3   = Dead
    -- Else, it stays alive.
    | otherwise                   = Alive
-- If a zombie has no alive neighboors, it dies, else it stays a zombie.
nextState Zombie neighboors = if countAlive neighboors == 0 then Dead else Zombie

-- | Makes 1 step forward, that is, advances the game one round
step :: Grid -> Grid
-- The next grid is just the current one, updated using f.
step grid = grid//[(pos,f pos) | pos <- indices grid] 
    where
        -- f takes an index, and calculates the next cell state of the cell at that index / Function definition
        f pos  = nextState (grid ! pos) (getNeighboorhood pos grid)

-- | Returns a String representation of the Grid.
showGame :: Grid -> String
-- Read last.
-- And now we can just map show to each row to a string (show), and join them using "\n"
showGame grid = unlines $ map show matrix
    where
        ((lowR,loC), (highR,highC)) = bounds grid
        -- We map the grid to the integer representation of each cell
        mapped    = toIntegerC <$> grid
        -- Then we transform the grid into a 1 dimensional list, by row
        listArray = elems mapped
        f xs      = if null xs then [] else take (highC+1) xs : f (drop (highC+1) xs) -- Function declaration
        matrix    = f listArray



-- | Recursive function that plays the game with n steps
gameOfLife :: Int -> Grid -> Grid

gameOfLife 0 grid = grid
gameOfLife n grid = gameOfLife (n-1) (step grid)

-- | Aux function to print the grid
printTable :: Grid -> IO ()
printTable = putStrLn . showGame

-- | Main function that reads the input in game.txt and outputs the result to out.txt
playFromFile :: IO ()
playFromFile = do
    -- Reads from the file in this order: iterations, rows, cols and then the grid
    (iterations:rows:cols:grid') <- map read . lines <$> readFile fileName
    let grid  = listArray ((0,0),(rows-1,cols-1)) $ map fromIntegerC grid'
    -- Calculates the final iteration
    let final = gameOfLife iterations grid
    -- Writes the final iteration to the out.txt file
    writeFile outName $ showGame final
    print "Solucao em out.txt"

main :: IO ()
main = playFromFile