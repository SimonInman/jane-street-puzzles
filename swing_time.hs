
import Data.Array
import Data.List
import Data.Function (on)
import Data.Functor
import Data.Maybe

--TODO create a utils file. Move things that don't need a grid?

--Grid Stuff
-- A square is an index, and True IFF there is a post there
type Index = (Int, Int)
type Square = (Index, Bool)
type Grid = Array Index Bool
data Direction = Clockwise | Anticlockwise deriving (Eq)
--rename?
data Cardinal = Up | Down | L | R deriving (Eq) --Left and Right are already in prelude :(
--ShortestPath stuff
type Cost = Float
--this will need to take into account the post to submit the answer
type Path = [(Index, Index)]
type VisitedNode = (Index, Cost, Path)
type UnvisitedNode = (Index, Maybe Cost, Path)

findShortestPath :: Index -> Index -> Grid -> VisitedNode
findShortestPath start end grid =
    let unvisitedIndices = delete start (indices grid)
        unvisitedNodes = [(i, Nothing, []) | i <- unvisitedIndices]
        dummyPost = (0,0)
    in shortestPath end [] ((start, Just 0, [(dummyPost, start)]):unvisitedNodes) grid

runFSP = findShortestPath (1,1) (20,20) realGrid

shortestPath :: Index -> [VisitedNode] -> [UnvisitedNode] -> Grid -> VisitedNode
shortestPath target visited unvisited grid
    | nextToVisitIndex == target = nowVisited
    --hmm do i actually need to track visited?
    | otherwise = shortestPath target (nowVisited:visited) newUnvisited grid
    where (nextToVisit : restToVisit) = sortByLeastCost unvisited
          (nextToVisitIndex,_shouldBeJust,_path) = nextToVisit
          (nowVisited, newUnvisited) = visitNode nextToVisit restToVisit grid

sortByLeastCost :: [UnvisitedNode] -> [UnvisitedNode]
sortByLeastCost = sortBy (flip compare `on` nodeKey)
	where nodeKey (_, cost, _) = (* (-1)) <$> cost

--can we get to the unvisited any quicker using currentlyVisiting?
--should rename this?
visitNode :: UnvisitedNode -> [UnvisitedNode] -> Grid -> (VisitedNode, [UnvisitedNode])
visitNode currentlyVisiting unvisited grid =
    let (visitingIndex, Just currentCost, currentPath) = currentlyVisiting
        nowVisited = (visitingIndex, currentCost, currentPath)
        possibleNext = reachable visitingIndex grid
        newUnvisited = map (maybeUpdateNode nowVisited possibleNext) unvisited
    in (nowVisited, newUnvisited)

--also we should be returning (PostLocation, StoppingPoint) for eaech move
reachable :: Index -> Grid -> [(Index, Index, Cost)]
reachable start grid =
    let allPosts = postLocations grid
        noPostinWay p = p == effectivePost start p grid
        reachablePosts = filter noPostinWay allPosts
        swingUsingPost p = canSwingTo start p grid
    in concatMap swingUsingPost reachablePosts

-- hmm running into horribleness around cost being a maybe type
--maybe just use something like -1 as a horrible sentinel value
maybeUpdateNode :: VisitedNode -> [(Index, Index, Cost)] -> UnvisitedNode-> UnvisitedNode
maybeUpdateNode currentNode reachableNodes unvisitedNode
    | shouldUpdate = updatedNode
    | otherwise = unvisitedNode
    where (unvisitedIndex, costToBeat, _pathToBeat) = unvisitedNode
          (currentIndex, currentCost, currentPath) = currentNode
          (shouldUpdate, viaPost, costFromCurrent) = shouldUpdateNode (currentCost,currentIndex) (costToBeat,unvisitedIndex) reachableNodes
          updatedNode = (unvisitedIndex, Just costFromCurrent, updatedPath)
          updatedPath = (viaPost, unvisitedIndex) : currentPath

--this has a silly return value :/
shouldUpdateNode :: (Cost, Index) -> (Maybe Cost, Index) -> [(Index, Index, Cost)] -> (Bool, Index, Cost)
shouldUpdateNode (currentCost, _currentIndex) (maybeUnvisitedCost, unvisitedIndex) reachableNodes
    | isUnreachable = (False, (0,0), 0)
    | otherwise = (isLowerCost, viaPost, costFromCurrent)
    where maybeTargetNode = filter (\(ind,_,_) -> ind == unvisitedIndex) reachableNodes
          isUnreachable = null maybeTargetNode
          (_targetIndex, viaPost, costToTarget) = head maybeTargetNode
          stepCost = sqrt costToTarget --distance is already the square of the actual rope length!
          costFromCurrent = currentCost + stepCost
          isLowerCost = isNothing maybeUnvisitedCost || (Just costFromCurrent < maybeUnvisitedCost)

--this check fails
goesOffGrid :: Index -> Index -> Float -> Direction -> Grid -> Maybe Float
goesOffGrid startLocation ropedPost startAngle direction grid =
    let toTest = cardinalDirections startAngle direction
        ropeLength = sqrt $ fromIntegral $ distance startLocation ropedPost
        gridSize = fromIntegral $ snd $ snd $ bounds grid
        f = outAtAngle ropedPost ropeLength direction gridSize
        limits = map f toTest
        realLimits = dropWhile (== Nothing) limits
    in if null realLimits then Nothing else head realLimits

outAtAngle :: Index -> Float -> Direction -> Float -> Cardinal -> Maybe Float
outAtAngle (ropedX, ropedY) ropeLength direction gridSize cardinal
    | ropeLength < edgeDist = Nothing
    | otherwise = Just $ outAtAngleHelper ropeLength edgeDist cardinal direction
    where edgeDist = distanceToGridEdge ropedFloat cardinal gridSize
          ropedFloat = (fromIntegral ropedX, fromIntegral ropedY)

outAtAngleHelper :: Float -> Float -> Cardinal -> Direction -> Float
outAtAngleHelper ropeLen gridEdgeDist R Anticlockwise =
    2*pi - acos (gridEdgeDist / ropeLen)
outAtAngleHelper ropeLen gridEdgeDist cardinal direction
    | cardinal == R = angleDiff
    | cardinal == Up = (pi/2) + angleDiff
    | cardinal == L = pi + angleDiff
    | cardinal == Down = (3*pi/2) + angleDiff
    where angleSize = acos (gridEdgeDist / ropeLen)
          angleDiff = if direction == Clockwise then angleSize else (-angleSize)

--expected
testOAA = let ropeLen = sqrt $ fromIntegral $ (distance (1,1) (19,2)) :: Float
          in [outAtAngle (19,2) ropeLen Clockwise 20 L,
              outAtAngle (19,2) ropeLen Clockwise 20 Up,
              outAtAngle (19,2) ropeLen Clockwise 20 R,
              outAtAngle (19,2) ropeLen Clockwise 20 Down ]

distanceToGridEdge :: (Float, Float) -> Cardinal -> Float -> Float
distanceToGridEdge (_across, up) Down _gridSize = up - 0.5
distanceToGridEdge (across, _up) L _gridSize = across - 0.5
distanceToGridEdge (_across, up) Up gridSize = gridSize - up + 0.5
distanceToGridEdge (across, _up) R gridSize = gridSize - across + 0.5

--what order will we encounter up, down, left and right in?
cardinalDirections :: Float -> Direction -> [Cardinal]
cardinalDirections startAngle Anticlockwise =
    reverse $ cardinalDirections startAngle Clockwise
cardinalDirections startAngle Clockwise
    | startAngle <= pi/2 = [R, Down, L, Up]
    | startAngle <= pi = [Up, R, Down, L]
    | startAngle <= 3*pi/2 = [L, Up, R, Down]
    | otherwise = [Down, L, Up, R]

--TODO change return to include ropedPost (Index, Index, Cost)
canSwingTo :: Index -> Index -> Grid -> [(Index, Index, Cost)]
canSwingTo start ropedPost grid =
    let directed = canSwingToDirected start ropedPost grid
        ropeLenSquared = distance start ropedPost
        cost = 1/fromIntegral ropeLenSquared
        landingSpots = directed Anticlockwise ++ directed Clockwise
    in zip3 landingSpots (repeat ropedPost) (repeat cost)

canSwingToDirected :: Index -> Index -> Grid -> Direction -> [Index]
canSwingToDirected start ropedPost grid direction =
    let direct = directLandingSquares start ropedPost direction grid
        lastPoint = if null direct then Nothing else Just (last direct)
        indirect = secondaryLandingSquares lastPoint ropedPost direction grid
    in (direct ++ indirect)

secondaryLandingSquares :: Maybe Index -> Index -> Direction -> Grid -> [Index]
secondaryLandingSquares Nothing _ _ _ = []
secondaryLandingSquares (Just lastLanding) ropedPost direction grid =
    let maybePostWeHit = effectivePost lastLanding ropedPost grid
        noSuchPole = maybePostWeHit == ropedPost
    in if noSuchPole then [] else canSwingToDirected lastLanding maybePostWeHit grid direction

--hm this seems like it should be easier?
effectivePost :: Index -> Index -> Grid -> Index
effectivePost inSquare ropedPost grid =
    let ourAngle = angle inSquare ropedPost
        posts = postLocations grid
        withAngles = attachAngles inSquare posts
        sameAngleFun (_, a) = a == ourAngle
        colinearPosts = map fst $ filter sameAngleFun withAngles
    in removeColinearPosts inSquare colinearPosts

--sorted list of legal landing spots in given direction
directLandingSquares :: Index -> Index -> Direction -> Grid -> [Index]
directLandingSquares startPos ropedPost direction grid =
      --get all the points that could ever work
    let withoutObstruction = removePosts $ pointsWithSameRadius startPos ropedPost grid
        startAngle = angle ropedPost startPos
        furthestAngle = furthestAngleAllowed startPos ropedPost startAngle direction grid
        landingsWithAngles = landingSquares ropedPost startAngle furthestAngle direction withoutObstruction
        sortedWithAngles = sortByAngle landingsWithAngles startAngle direction
   in map fst sortedWithAngles

--how far can we swing before hitting a post or an edge?
furthestAngleAllowed :: Index -> Index -> Float -> Direction -> Grid -> Maybe Float
furthestAngleAllowed startPos ropedPost startAngle direction grid =
    -- get all the posts that could be in the way
    let possiblyBlockingPosts = postsInRadius startPos ropedPost grid
        maybePostAndAngle = firstPostAndAngle ropedPost startAngle possiblyBlockingPosts direction
        maybePostAngle = getAngle maybePostAndAngle
        maybeOffGrid = goesOffGrid startPos ropedPost startAngle direction grid
        --This shouldn't be a min. and also, it's unused. run compiler with some warnings agian. 
    --in  maybeMaxAngle = min maybeAngle maybeOffGrid
    in firstAngleInDirection startAngle direction maybePostAngle maybeOffGrid

firstAngleInDirection :: Float -> Direction -> Maybe Float -> Maybe Float -> Maybe Float
firstAngleInDirection _ _ first Nothing = first
firstAngleInDirection _ _ Nothing second = second
firstAngleInDirection startAngle direction (Just first) (Just second) =
    Just $ normaliseAngle (normedAns + startAngle)
    where normalisedFirst= normaliseAngle (first - startAngle)
          normalisedSecond= normaliseAngle (second - startAngle)
          normedAns = case direction of
              Clockwise -> max normalisedFirst normalisedSecond
              Anticlockwise -> min normalisedFirst normalisedSecond

normaliseAngle ang
    | ang < 0 = normaliseAngle (ang + (2*pi))
    | ang >= 2*pi = normaliseAngle (ang - (2*pi))
    | otherwise = ang

sortByAngle :: [(Index, Float)] -> Float -> Direction -> [(Index, Float)]
sortByAngle indicesWithAngles startAngle direction =
    let ordered = sortBy (compare `on` snd) indicesWithAngles
        (before, after) = span (\(_,ang) -> ang < startAngle) ordered
        anticlockwise = after ++ before
    in if direction == Anticlockwise then anticlockwise else reverse anticlockwise

--TODO that this exists is a code smell
getAngle :: Maybe (Index,Float) -> Maybe Float
getAngle Nothing = Nothing
getAngle (Just (_ind, ang)) = Just ang

removePosts :: [Square] -> [Index]
removePosts sqs =
    let withoutPosts = filter (\(_,isPost) -> not isPost) sqs
    in map fst withoutPosts

attachAngles :: Index -> [Index] -> [(Index, Float)]
attachAngles startSq targetSqs =
    let angles = map (angle startSq) targetSqs
    in zip targetSqs angles

landingSquares  :: Index -> Float -> Maybe Float -> Direction -> [Index] -> [(Index, Float)]
landingSquares _ _ _ _ [] = []
landingSquares ropedPost startAngle maxAngle direction possibleSquares =
    filter isBetween squaresAndAngles
    where squaresAndAngles = attachAngles ropedPost possibleSquares
          isBetween (_,ang) = betweenAngles startAngle maxAngle ang direction

--Is a given angle between a start angle and a possible limit.
--Accounting for wrapping at 2pi. This is inclusive of the borders
betweenAngles :: Float -> Maybe Float -> Float -> Direction -> Bool
betweenAngles _ Nothing _ _ = True
betweenAngles startAngle (Just maxAngle) angleUnderTest dir
    | startAngle == angleUnderTest = True
    | startAngle == maxAngle = True
    | dir == Clockwise = not isAntiClockwise
    | dir == Anticlockwise = isAntiClockwise
    where isAntiClockwise = betweenAnglesAntiClockwise startAngle maxAngle angleUnderTest

--this seems like it could be simplified
betweenAnglesAntiClockwise :: Float -> Float -> Float -> Bool
betweenAnglesAntiClockwise startAngle maxAngle angleUnderTest
    | maxAngle > startAngle = (startAngle <= angleUnderTest) && (angleUnderTest <= maxAngle)
    | maxAngle == startAngle = startAngle == angleUnderTest
    | maxAngle < startAngle = (angleUnderTest <= maxAngle) || (angleUnderTest >= startAngle)

firstPostAndAngle :: Index -> Float -> [Index] -> Direction -> Maybe (Index, Float)
firstPostAndAngle _ _ [] _ = Nothing
firstPostAndAngle ropedPost startAngle posts direction =
    let anglesWithSquare = attachAngles ropedPost posts
        sortedbyAngle = sortByAngle anglesWithSquare startAngle direction
    in Just $ head sortedbyAngle

postsInRadius :: Index -> Index -> Grid -> [Index]
postsInRadius start post grid =
    let dist = distance start post
        filterFun maybePost = isPostInRadius post maybePost dist
        squares = filter filterFun (assocs grid)
    in map fst squares

--is a given square a post, and is it within distance of where
--we're swinging from?
isPostInRadius :: Index -> Square -> Int -> Bool
isPostInRadius ropedPost possiblePost maxDist =
    let (endLocation, isPost) = possiblePost
        ourDist = distance ropedPost endLocation
    in isPost && (maxDist >= ourDist) && (ourDist /= 0)

pointsWithSameRadius :: Index -> Index -> Grid -> [Square]
pointsWithSameRadius start post grid =
    let desiredDist = distance start post
        isSameDist ind = desiredDist == distance ind post
        filterFun (ind, _) = (ind /= start) && isSameDist ind
    in filter filterFun (assocs grid)

--clockwise angle to one point from another
-- angle (0,0) (1,1) = 45 degrees or pi/4
angle :: Index -> Index -> Float
angle (x1, y1) (x2, y2)
    | x1 == x2 && y2 > y1 = pi/2
    | x1 == x2 && y2 < y1 = 3*pi/2
    | x2 < x1 = pi + atan (fromIntegral oppSide/ fromIntegral adjSide)
    | x2 > x1 && y2 < y1 = 2*pi + atan (fromIntegral oppSide/ fromIntegral adjSide)
    | otherwise = atan (fromIntegral oppSide/ fromIntegral adjSide)
    where oppSide = y1 - y2
          adjSide = x1 - x2

--returns square of distance - avoids need for floats
distance :: Index -> Index -> Int
distance (x1, y1) (x2,y2) = (x1 - x2)^2 + (y1-y2)^2

postLocations :: Grid -> [Index]
postLocations grid =
    let posts = filter snd $ assocs grid
    in map fst posts

--from a set of poles with the same angle, get the nearest.
removeColinearPosts :: Index -> [Index] -> Index
removeColinearPosts startSquare posts =
    minimumBy (compare `on` distance startSquare) posts

testGrid :: Grid
testGrid = array ((1,1),(12,12)) g
    where trueSquares = [ (9,7),(6,4), (7,3)]
          g = [ ((x,y), (x,y) `elem` trueSquares) | x <- [1..12], y <- [1..12]]

realGrid :: Grid
realGrid = array ((1,1),(20,20)) g
    where trueSquares = [
                         (5,20),(12,20),(14,20),
                         (3,19),(7,19),(9,19),(19,19),
                         (15,18),
                         (5,17),(7,17),(13,17),(16,17),
                         (4,16),(8,16),(11,16),(19,16),
                         (3,15),(12,15),(20,15),
                         --
                         (2,13),(10,13),(14,13),
                         (4,12),(11,12),(17,12),
                         (1,11),(13,11),(19,11),
                         (2,10),(9,10),
                         (4,9),(6,9),(11,9),
                         (9,8),
                         (1,7),(20,7),
                         --
                         (2,5),(12,5),(20,5),
                         (1,4),(3,4),(8,4),(14,4),
                         (10,3),
                         (7,2),(11,2),(19,2),
                         (3,1),(5,1),(15,1) ]
          g = [ ((x,y), (x,y) `elem` trueSquares) | x <- [1..20], y <- [1..20]]

prettyPrint :: (Index, Cost, Path) -> String
prettyPrint (target, cost, path) =
    let intro = "shortest path to " ++ show target ++ " in cost " ++ show cost ++ "is\n"
    in intro ++ prettyPrintPath path

prettyPrintPath :: Path -> String
prettyPrintPath path =
    let ((_,start) : inOrder) = reverse path
        startStr = "Starting at " ++ show start ++ "\n"
        moveString (rope, landing) = "Use " ++ show rope ++ " to swing to " ++ show landing ++ "\n"
    in startStr ++ (concatMap moveString inOrder)

--main :: IO ()
main = putStr $ prettyPrint $ findShortestPath (1,1) (20,20) realGrid
