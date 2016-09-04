import Data.Matrix
import qualified Data.Vector as Vector
import Math.NumberTheory.Primes.Factorisation
--import Test.QuickCheck
import Data.Maybe
--import Data.Function (on)
import Data.List

data Piece = Queen | King | Rook | Bishop | Knight deriving (Show, Eq)
type Row = Int
type Column = Int
type Square = (Row, Column)

type PiecePosition = (Piece, Square)
type SquareValue = Maybe Int --0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
type Board = Matrix SquareValue
data State = State [PiecePosition] Board Targets deriving (Show)
type RowProducts = [Int]
type ColProducts = [Int]
--data Target = RowProducts prod | ColProducts prod
type Targets = (RowProducts, ColProducts)

afterNMovesHelper :: Int -> Int -> [State] -> [State]
afterNMovesHelper target counter states
    | target > counter = states
    | otherwise = afterNMovesHelper target (counter - 1) (concatMap (nextStates counter) states)

nextStates :: Int -> State -> [State]
nextStates moveNum state =
    filter isValidState $ allNextStates moveNum state

allNextStates :: Int -> State -> [State]
allNextStates moveNum (State positions board targets) =
        map (updateState targets board moveNum) validNextPositions
    where
        allNextPositions = sequence $ map (nextMoves board) positions
        validNextPositions = filter isValidPosition allNextPositions

isValidState :: State -> Bool
isValidState (State _ board (targetRowProds, targetColProds)) =
    allDivisible rowZipped && allDivisible colZipped
    where
        rowZipped = zip targetRowProds currentRowProds
        colZipped = zip targetColProds currentColProds
        currentRowProds = rowProducts board
        currentColProds = colProducts board
        allDivisible = all (\(target,curr) -> target `rem` curr == 0)

isValidPosition :: [PiecePosition] -> Bool
isValidPosition pPositions =
    (not $ anyUnderAttack pPositions)
    where
        --notCollided = positions == nub positions --already doing this in underAttack
        positions = map snd pPositions :: [Square]

anyUnderAttack :: [PiecePosition] -> Bool
anyUnderAttack [_] = False
anyUnderAttack (x:xs) = any (eitherUnderAttack x) xs || anyUnderAttack xs

eitherUnderAttack :: PiecePosition -> PiecePosition -> Bool
eitherUnderAttack (p1, sq1) (p2, sq2) =
    isValidMove (p1,sq1) sq2 || isValidMove (p2, sq2) sq1

nextMoves :: Board -> PiecePosition -> [PiecePosition]
nextMoves board pp@(piece,_) = zip (repeat piece) moves
    where
        moves = filter (isValidMove pp) $ emptySquares board

emptySquares :: Board -> [Square]
emptySquares board =
    [sq | sq <- boardSquares, isNothing $ board ! sq]

isValidMove :: PiecePosition -> Square -> Bool
isValidMove (piece, start) sq =
    case piece of
        Queen -> isValidMove (Rook, start) sq || isValidMove (Bishop, start) sq
        King -> rDiff < 2 && cDiff < 2
        Rook -> rDiff == 0 || cDiff == 0
        Bishop -> rDiff == cDiff
        Knight -> (rDiff + cDiff) == 3 && (rDiff == 1 || cDiff == 1)
    where
        rDiff = rowDiff start sq
        cDiff = colDiff start sq

rowDiff :: Square -> Square -> Int
rowDiff (startR, _) (r, _) = abs (startR - r)
colDiff :: Square -> Square -> Int
colDiff (_, startC) (_, c) = abs (startC - c)

--
-- Attempt at backwards approach
--

--board position, i.e. where the 7s are, ignoring pieces

statesAt7moves :: [State]
statesAt7moves = map (updateState t b 7 ) possibleAt7moves
    where
        State _ b t = initialState

possibleAt7moves :: [[PiecePosition]]
possibleAt7moves = filter isValidPosition allPos
    where
        allPos = [zip piecePerms sqs | piecePerms <- permutations allPieces, sqs <- sevenLocations]

type Possible7 = [Square]
sevenLocations :: [Possible7]
sevenLocations = filter noOverlaps noRepeats
    where
        noOverlaps possible = [] == (possible `intersect` map snd startPositions)
        noRepeats = nubBy (\a b -> sort a == sort b) allPerms
        allPerms = map (zip row7s) $ permutations col7s
        row7s = getWhereSevensAre targetRowProducts
        col7s = getWhereSevensAre targetColProducts

getWhereSevensAre :: [Int] -> [Int]
getWhereSevensAre targets =
    toSparseForm 1 [] $ map (toDividesSeven . factorise . fromIntegral ) targets
    where
    --TODO this is horrible and nongeneral
        toDividesSeven factors = fromMaybe 0 $ lookup 7 factors
        toSparseForm _n acc [] = acc
        toSparseForm n acc (0:rest) = toSparseForm (n+1) acc rest
        toSparseForm n acc (k:rest) = toSparseForm (n+1) (replicate k n ++ acc) rest

isValidFirstPosition :: State -> Bool
isValidFirstPosition (State pps _ _) = all canMove pps
    where
        canMove pp@(piece, _) = case initial piece of
                                    Nothing -> False -- should happen - assert instead?
                                    Just sq -> isValidMove pp sq
        initial piece = lookup piece startPositions


backtracksToZero :: State -> [State]
backtracksToZero s = filter isValidFirstPosition $ afterNMovesHelper 1 6 [s]

--this gives you the sevens
sevenWithValidBacktrack :: State
sevenWithValidBacktrack =
    State pp7 board7 t
    where
        --candidate@(State pp7 _ t) = assert (lenght sevensWithValidBacktrack = 1) $ head sevensWithValidBacktrack
        --break if more than one as i think there shold only be one
        [State _ board7 _] = backtracksToZero candidate
        [candidate@(State pp7 _ t)] = candidates
        candidates = filter hasBacktrack statesAt7moves
        hasBacktrack state = not (null (backtracksToZero state))

productsSum :: State -> Int
productsSum (State _ board _ ) =
    sum (rowProducts board) + sum (colProducts board)

--after 7 is 18550
solve :: Int
solve = maximum $ map productsSum possible8s
    where
    --nextStates does all the checking for products...
        possible8s = allNextStates 8 sevenWithValidBacktrack

--
-- Initialisation and utils
--
updateState :: Targets -> Board -> Int -> [PiecePosition] -> State
updateState t board moveNumber pps =
    State pps newBoard t
    where
        newBoard = foldl updateBoard board newValues
        position = map snd pps
        newValues = zip position $ repeat moveNumber

updateBoard :: Board -> (Square, Int) -> Board
updateBoard board (sq, moveNum) = setElem (Just moveNum) sq board

initialState :: State
initialState = updateState (targetRowProducts, targetColProducts) blankBoard 0 startPositions

boardSquares :: [(Row, Column)]
boardSquares = [(r,c) | r <- [1..8], c <- [1..8]]

blankBoard :: Board
blankBoard = fromList 8 8 $ repeat Nothing

startPositions :: [PiecePosition]
startPositions =
    [(Queen,  (1,2)),
     (King,   (2,8)),
     (Rook,   (5,5)),
     (Bishop, (8,7)),
     (Knight, (7,1))]

rowProducts :: Board -> RowProducts
rowProducts board = map (vectorProd . row) [1..8]
    where
        row r = getRow r board

colProducts :: Board -> ColProducts
colProducts board  = map (vectorProd . col) [1..8]
    where
        col c = getCol c board

vectorProd :: Vector.Vector (Maybe Int) -> Int
vectorProd v = Vector.product unwrapped
    where
        unwrapped = Vector.map unwrap v
        unwrap val = case val of
                         Nothing -> 1
                         Just 0 -> 1
                         Just n -> n

targetRowProducts :: [Int]
targetRowProducts = [7,1890,8,10080,20,840,144,1260]
targetColProducts :: [Int]
targetColProducts = [2744,36,375, 336,108,240,20,504]

allPieces :: [Piece]
allPieces = [Queen, King, Rook, Bishop, Knight]
--TESTING
--testIsValidMove :: Property
--testIsValidMove = forAll (elements [Queen, King, Rook, Bishop, Knight]) $ \p ->
--                  forAll (elements boardSquares) $ \sq -> moveIsReversable (p, sq)
--    where
--        moveIsReversable pp = pp `elem` nextNextMoves pp
--        nextNextMoves pp = concatMap (nextMoves blankBoard) (nextMoves blankBoard pp)

main :: IO ()
--main = putStr $ show $ sevenWithValidBacktrack
--main = putStr $ show $ nextStates 8 candidates $ head sevensWithValidBacktrack
main = putStr $ show solve
