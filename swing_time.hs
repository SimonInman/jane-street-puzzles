
import Data.Array
import Data.List
import Data.Function (on)
import Data.Functor
import Data.Maybe

--TODO create a utils file. Move things that don't need a grid?

--Grid Stuff
-- A square is an index, and True IFF there is a post there
type Index = (Int, Int)
type Post = Index
type Square = (Index, Bool)
type Angle = Float
type RopeLength = Float --TODO this is currently the square, must become the real one
type Grid = Array Index Bool
data Direction = Clockwise | Anticlockwise deriving (Eq)
data SwingStart = SwingStart Post Angle RopeLength
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
    let reachablePosts = filter (isReachablePost grid start) $ postLocations grid
        swingUsingPost p = canSwingTo start p grid
    in concatMap swingUsingPost reachablePosts

isReachablePost :: Grid -> Index -> Post -> Bool
isReachablePost grid start targetPost =
    let ang = angle targetPost start
        dist = distance targetPost start
    in targetPost == effectivePost (SwingStart targetPost ang dist) grid

testIRP = isReachablePost testGrid (8,2) (6,4)

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
goesOffGrid :: SwingStart -> Direction -> Grid -> Maybe Float
goesOffGrid (SwingStart ropedPost startAngle ropeLen) direction grid =
    let toTest = cardinalDirections startAngle direction
        gridSize = fromIntegral $ snd $ snd $ bounds grid
        f = outAtAngle ropedPost ropeLen direction gridSize
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
testOAA = let ropeLen = distance (1,1) (19,2) :: Float
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

canSwingTo :: Index -> Post -> Grid -> [(Index, Post, Cost)]
canSwingTo startPos ropedPost grid =
    let ropeLen = distance startPos ropedPost
        startAngle  = angle ropedPost startPos
        directed = canSwingToDirected (SwingStart ropedPost startAngle ropeLen) grid
        cost = 1/(ropeLen^2)
        landingSpots = directed Anticlockwise ++ directed Clockwise
    in zip3 landingSpots (repeat ropedPost) (repeat cost)

canSwingToDirected :: SwingStart -> Grid -> Direction -> [Index]
canSwingToDirected swingStart grid direction =
    let direct = directLandingSquares swingStart direction grid
        possiblyBlockingPosts = postsInRadius swingStart grid
        maybePostAndAngle = angleAndIndexFirstBlockingPost swingStart possiblyBlockingPosts direction
    in case maybePostAndAngle of
           Nothing -> direct
           Just (post, ang) -> let indirect = secondaryLandingSquares swingStart post ang direction grid
                              in (direct ++ indirect)

secondaryLandingSquares :: SwingStart -> Post -> Angle -> Direction -> Grid -> [Index]
secondaryLandingSquares (SwingStart originalRopedPost _ ropeLen) newPost newAng direction grid =
    let newDist = ropeLen - distance originalRopedPost newPost
        newSwingStart = SwingStart newPost newAng newDist
    in canSwingToDirected newSwingStart grid direction

--Did we snag another post?
effectivePost :: SwingStart -> Grid -> Index
effectivePost (SwingStart ropedPost currentAngle ropeLen) grid =
    let allPosts = postLocations grid
        isBetween post = (angle ropedPost post == currentAngle) && (distance post ropedPost < ropeLen)
        candidates = filter isBetween allPosts
    in case candidates of
           [] -> ropedPost
           _ -> maximumBy (compare `on` distance ropedPost) candidates

--sorted list of legal landing spots in given direction
directLandingSquares :: SwingStart -> Direction -> Grid -> [Index]
directLandingSquares swingStart@(SwingStart ropedPost startAngle ropeLen) direction grid =
      --get all the points that could ever work
    let withoutObstruction = pointsWithSameRadius ropedPost ropeLen grid
        furthestAngle = furthestAngleAllowed swingStart direction grid
        landingsWithAngles = landingSquares ropedPost startAngle furthestAngle direction withoutObstruction
        sortedWithAngles = sortByAngle landingsWithAngles startAngle direction
   in map fst sortedWithAngles

--how far can we swing before hitting a post or an edge?
furthestAngleAllowed :: SwingStart -> Direction -> Grid -> Maybe Float
furthestAngleAllowed sS@(SwingStart _ startAngle _) direction grid =
    -- get all the posts that could be in the way
    let possiblyBlockingPosts = postsInRadius sS grid
        maybePostAngle = snd <$> angleAndIndexFirstBlockingPost sS possiblyBlockingPosts direction
        maybeOffGrid = goesOffGrid sS direction grid
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

normaliseAngle :: Angle -> Angle
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

angleAndIndexFirstBlockingPost :: SwingStart -> [Index] -> Direction -> Maybe (Index, Float)
angleAndIndexFirstBlockingPost _ [] _ = Nothing
angleAndIndexFirstBlockingPost (SwingStart ropedPost startAngle _) posts direction =
    let anglesWithSquare = attachAngles ropedPost posts
        sortedbyAngle = sortByAngle anglesWithSquare startAngle direction
    in Just $ head sortedbyAngle

postsInRadius :: SwingStart -> Grid -> [Index]
postsInRadius (SwingStart post _ ropeLen) grid =
    let filterFun maybePost = isPostInRadius post maybePost ropeLen
        squares = filter filterFun (assocs grid)
    in map fst squares

--is a given square a post, and is it within distance of where
--we're swinging from?
isPostInRadius :: Post -> Square -> Float -> Bool
isPostInRadius ropedPost possiblePost maxDist =
    let (endLocation, isPost) = possiblePost
        ourDist = distance ropedPost endLocation
    in isPost && (maxDist >= ourDist) && (ourDist /= 0)

pointsWithSameRadius :: Index -> RopeLength -> Grid -> [Index]
pointsWithSameRadius post desiredDist grid =
    let isSameDist ind = desiredDist == distance ind post
        filterFun (ind, isPost) = isSameDist ind && not isPost
    in map fst $ filter filterFun (assocs grid)

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
distance :: Index -> Index -> RopeLength
distance (x1, y1) (x2,y2) = sqrt $ fromIntegral squaredLen
    where squaredLen = ((x1 - x2)^2 + (y1-y2)^2) :: Int

postLocations :: Grid -> [Index]
postLocations grid =
    let posts = filter snd $ assocs grid
    in map fst posts

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
    in startStr ++ concatMap moveString inOrder

main :: IO ()
main = putStr $ prettyPrint $ findShortestPath (1,1) (20,20) realGrid
