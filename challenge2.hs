import Data.List (find)
import Debug.Trace (trace)

data EdgeDir = L | R deriving (Eq, Show, Ord)
data Face = East | West | South deriving (Eq, Show, Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq, Show, Ord)
type Atom = (Int, Int)

-- Calculate all ray interactions (entry and exit points) for the grid with given atoms
calcInteractions :: Int -> [Atom] -> [(EdgePoint, EdgePoint)]
calcInteractions gridSize atoms =
  let
    -- Generate all possible entry points on the grid edges
    allEntryPoints :: Int -> [(EdgePoint, Int)]
    allEntryPoints size = concatMap faceEntryPoints [East, West, South]
      where
        -- Generate entry points for a specific face
        faceEntryPoints East  = [(EP East i d, 2 * i - 1) | i <- [1..size], d <- [L, R]]
        faceEntryPoints West  = [(EP West i d, 1)         | i <- [1..size], d <- [L, R]]
        faceEntryPoints South = [(EP South i d, 2 * i - 1) | i <- [1..size], d <- [L, R]]

    -- Adjust entry points, modifying South entries to start at the bottom row
    adjustEntry :: EdgePoint -> EdgePoint
    adjustEntry (EP South _ dir) = EP South gridSize dir -- Change row to the bottom-most row
    adjustEntry ep = ep -- Keep other entry points unchanged

    -- Process a single entry point to compute its corresponding exit point
    processEntry :: (EdgePoint, Int) -> (EdgePoint, EdgePoint)
    processEntry (entry, startOffset) =
      (entry, traceRay gridSize atoms (adjustEntry entry, startOffset))

  in map processEntry (allEntryPoints gridSize) -- Map each entry point to its exit point

-- Determine the next position of a ray based on its current face, row, offset, and direction
move :: Face -> Int -> Int -> EdgeDir -> Maybe (Int, Int)
move face row offset dir =
  let
    -- Logic for moving on the East face
    eastMove L = Just (row, offset - 1)  -- Horizontal left
    eastMove R = Just (row + if odd offset then 1 else 0, offset + if odd offset then 1 else -1)  -- Diagonal right

    -- Logic for moving on the West face
    westMove L = Just (row + if odd offset then 1 else 0, offset + 1)  -- Diagonal left
    westMove R = Just (row, offset + 1)  -- Horizontal right

    -- Logic for moving on the South face
    southMove L = Just (row + if odd offset then 0 else -1, offset + if odd offset then 1 else -1)  -- Diagonal up-left
    southMove R = Just (row + if odd offset then 0 else -1, offset + if odd offset then -1 else -1)  -- Diagonal up-right

    -- Select the appropriate movement logic based on the face
    directionMove East = eastMove
    directionMove West = westMove
    directionMove South = southMove
  in (directionMove face) dir -- Apply movement logic for the given face

-- Compute the reflection of a ray when it hits an atom, returning the new face and direction
reflect :: Face -> EdgeDir -> (Int, Int) -> (Face, EdgeDir)
reflect face dir (row, offset) =
  let
    oddOffset = odd offset -- Check if the offset is odd

    -- Reflection logic for East face
    eastReflect L
      | oddOffset = (South, L)
      | otherwise = (West, L)
    eastReflect R
      | oddOffset = (West, R)
      | otherwise = (South, R)

    -- Reflection logic for West face
    westReflect L
      | oddOffset = (East, L)
      | otherwise = (South, L)
    westReflect R
      | oddOffset = (South, R)
      | otherwise = (East, R)

    -- Reflection logic for South face
    southReflect L
      | oddOffset = (West, L)
      | otherwise = (East, L)
    southReflect R
      | oddOffset = (East, R)
      | otherwise = (West, R)

    -- Select reflection logic based on the face
    faceReflect East = eastReflect
    faceReflect West = westReflect
    faceReflect South = southReflect
  in (faceReflect face) dir -- Apply reflection logic

-- Compute the exit point of a ray after it exits the grid
isExit :: Face -> Int -> EdgeDir -> EdgePoint
isExit face index dir =
  let
    -- Exit logic for East face
    exitForEast L = EP West index R -- Exits as West Right
    exitForEast R = EP South ((index + 1) `div` 2) L -- Exits as South Left

    -- Exit logic for West face
    exitForWest L = EP South ((index + 1) `div` 2) R -- Exits as South Right
    exitForWest R = EP East index L -- Exits as East Left

    -- Exit logic for South face
    exitForSouth L = EP East index R -- Exits as East Right
    exitForSouth R = EP West index L -- Exits as West Left

    -- Select exit logic based on the face
    chooseExit East = exitForEast
    chooseExit West = exitForWest
    chooseExit South = exitForSouth
  in (chooseExit face) dir -- Compute the exit point

-- Trace a ray through the grid, determining its exit point
traceRay :: Int -> [Atom] -> (EdgePoint, Int) -> EdgePoint
traceRay gridSize atoms (startPoint@(EP face row dir), offset) =
  let
    -- Recursive helper function for tracing the ray's path
    trace :: Face -> Int -> Int -> EdgeDir -> EdgePoint
    trace currentFace currentRow currentOffset currentDir
      -- Check if the ray is out of bounds
      | outOfBounds currentFace currentRow currentOffset =
          findExitPoint currentFace currentRow currentOffset currentDir

      -- Handle collision with an atom
      | (currentRow, currentOffset) `elem` atoms =
          let (newFace, newDir) = redirectRay currentFace currentDir (currentRow, currentOffset)
          in case nextMove newFace currentRow currentOffset newDir of
               Nothing -> EP newFace currentRow newDir -- Ray stops
               Just (nextRow, nextOffset) -> trace newFace nextRow nextOffset newDir -- Continue tracing

      -- Continue the ray's path if no collision
      | otherwise =
          case nextMove currentFace currentRow currentOffset currentDir of
            Nothing -> EP currentFace currentRow currentDir -- No valid step
            Just (nextRow, nextOffset)
              | outOfBounds currentFace nextRow nextOffset ->
                  findExitPoint currentFace nextRow nextOffset currentDir -- Exit the grid
              | otherwise -> trace currentFace nextRow nextOffset currentDir -- Continue tracing

    -- Check if a position is out of bounds
    outOfBounds :: Face -> Int -> Int -> Bool
    outOfBounds _ row offset = row <= 0 || row > gridSize || offset <= 0 || offset > row * 2 - 1

    -- Determine the exit point of a ray when it leaves the grid
    findExitPoint :: Face -> Int -> Int -> EdgeDir -> EdgePoint
    findExitPoint f r o d = 
      let exitIndex = case f of
            East  -> if d == R then o else r
            West  -> if d == L then o else r
            South -> r -- South exit depends only on the row
      in isExit f exitIndex d

    -- Compute the ray's next step
    nextMove :: Face -> Int -> Int -> EdgeDir -> Maybe (Int, Int)
    nextMove = move

    -- Redirect the ray after hitting an atom
    redirectRay :: Face -> EdgeDir -> (Int, Int) -> (Face, EdgeDir)
    redirectRay = reflect

  in trace face row offset dir -- Start tracing the ray

-- Generate combinations of atom positions
generateAtomCombinations :: Int -> Int -> [[Atom]]
generateAtomCombinations gridSize nAtoms =
  combinations nAtoms [(row, col) | row <- [1..gridSize], col <- [1..2*row-1]]

-- Helper to generate combinations
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) =
  [x : rest | rest <- combinations (n-1) xs] ++ combinations n xs

-- Check if the placement of atoms is valid
isValidPlacement :: Int -> [(EdgePoint, EdgePoint)] -> [Atom] -> Bool
isValidPlacement gridSize interactions atoms =
  calcInteractions gridSize atoms == interactions

-- Main solver function
solveTBB :: Int -> [(EdgePoint, EdgePoint)] -> [Atom]
solveTBB nAtoms interactions =
  let
    gridSize = maximum [i | EP _ i _ <- concatMap (\(EP f1 i1 _, EP f2 i2 _) -> [EP f1 i1 L, EP f2 i2 L]) interactions]
    allCombinations = generateAtomCombinations gridSize nAtoms
  in
    case find (isValidPlacement gridSize interactions) allCombinations of
      Just solution -> solution
      Nothing -> error "No valid solution found"