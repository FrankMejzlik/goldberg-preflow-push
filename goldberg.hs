
module Goldberg where

{-
The main source file for highest-label push-relabel (Goldberg) algorithm.

Entry point function is `main`.

IMPLEMENTATION:
    The algorithm `goldberg(FlowNet net)` is divided into two separate phases:
        1) `initialize` - Intializes the heights and sends the first preflow (wave) through outcoming edges of the SOURCE (`s`) vertex.
        2) Until there are same ACTIVE nodes, do `processVertex(GoldNet net, Vertex v)`

DATA TYPES:
    FlowNet - Flow network (G, s, t, c) where we use G as list of neighbours where each edge also holds capacity ().

-}

-- Third party libs
import Text.Pretty.Simple -- USAGE: stack ghci --package pretty-simple ./main.hs
import Data.Maybe

-- My modules
import Types

{- Initializes the user provided network for the Goldberg algorithm -}
goldbergInitialize net@(N vs es s t) = foldr (flip push) (toComputeNetwork net) (getRawOutEdges es s)

getRawOutEdges :: [E] -> Int -> [(VertId, VertId)]
getRawOutEdges es from = map (\(E fr to _) -> (fr, to)) (filter (\(E fr to _) -> fr == from) es)

{- Converts user network to representation for the algorithm calculations -}
toComputeNetwork :: N -> Network
toComputeNetwork (N vs es s t) = Network vss ess s t (replicate (length vs * 2 + 1) [])
  where
    vss = map fv vs -- Edgetended vertices
    ess = map fe es  ++ map feRev es -- Convert edges and add their reverses
    fv v@(V name)
        | name == s = Vertex name (length vs) capSum (map (\(E fr to c) -> (fr, to)) (filter (\ (E fr _ _) -> fr == name) es)  )
        | name == t = Vertex name 0 0.0 []
        | otherwise = Vertex name 0 0.0 []
    
    fe (E fr to c) = Edge fr to c 0.0
    feRev (E fr to c) = Edge to fr c 0.0

    capSum = sum (map (\(E _ _ c) -> c) es) -- Sum of all edge capacities

{- Gets only normal vertices from the network -}
normVertices (Network vs es s t _) = filter (\(Vertex n h ex des) -> n /= s && n /= t) vs


runGoldbergIterN net n = iterate goldbergStep initNet !! n
    where   
        -- Convert and initialize the algorithm
        initNet = goldbergInitialize net

runGoldberg net = until goldbergShouldTerminate goldbergStep initNet
    where   
        -- Convert and initialize the algorithm
        initNet = goldbergInitialize net

{- Returns True if the Goldberg algorithm is finished, False otherwise. -}
goldbergShouldTerminate :: Network -> Bool
goldbergShouldTerminate net@(Network vs es s t bucketQueue) 
    | isNothing $ getHighestVert bucketQueue = True
    | otherwise = False

goldbergStep :: Network -> Network
goldbergStep net@(Network vs es s t netQ) = if not (null descEdges) -- If no pushable edge found
    then push net (head descEdges) -- We can push => push 
    else liftVertex net vertId 1 -- Cannot push => lift by 1
        where
            (Vertex _ _ _ descEdges) = findVertex vs vertId
            vertId = unJust $ getHighestVert netQ


getHighestVert :: [[VertId]] -> Maybe VertId
getHighestVert (k : lowerKs) = if not (null k) -- If non-empty list
    then Just (head k) -- We found it
    else getHighestVert lowerKs -- Try lower buckets

getHighestVert [] = Nothing -- Recurse bottom => means not found

{- Pushes possible maximum through the provided edge -}
push :: Network -> (VertId, VertId) -> Network
push net@(Network vs es s t netQ) (fr, to) = pushedNet
  where
    pushedNet = Network (newFromVert : newToVert : restVerts) (newEdge : newEdgeRev : restEdges) s t newQ
    
    -- Amount to push 
    diff = min fromEx edgeRes
    isSaturPush = edgeRes == diff
    
    -- Handle queue properly
    newQ = updateQueue netQ diff fromVert toVert s t
    
    -- Old vertices
    fromVert@(Vertex frId frH fromEx frDes) = findVertex vs fr
    toVert@(Vertex toId toH toEx toDes) = findVertex vs to
    restVerts = filter (\(Vertex name _ _ _) -> name /= fr && name /= to) vs
    
    -- New vertices 
    newFromVert = Vertex frId frH (fromEx - diff) [ newD | newD@(eFromId, eToId) <- frDes, (not isSaturPush || eToId /= toId) ]
    newToVert = Vertex toId toH (toEx + diff) toDes
    
    -- Old edges
    edgeRes = eCap - eFlow
    foundEdge@(Edge eFr eTo eCap eFlow) = findEdge es fr to
    restEdges = filter (\(Edge _fr _to _ _) -> not ( (_fr == fr || _fr == to) && (_to == fr || _to == to) ) ) es
    
    -- New edges
    newEdge = Edge eFr eTo eCap (eFlow + diff)
    newEdgeRev = Edge eTo eFr eCap ((-eFlow) - diff)

{- Updates the bucket queue after the push based on provided arguments. -}
updateQueue :: [[VertId]] -> Rational -> Vertex -> Vertex -> VertId -> VertId -> [[VertId]]
updateQueue netQ diff vFrom@(Vertex frId frH frEx frDes) vTo@(Vertex toId toH toEx toDes) s t = newNetQ
  where
    cleanQueue = getCleanQueue netQ frId toId
    
    -- Decide if FROM vertex should belong to the Q (s,t excluded)
    fromVertexQueue = [frId | frEx - diff > 0 && frId /= s && frId /= t]
    -- Decide if TO vertex should belong to the Q (s,t excluded)
    toVertexQueue = [toId | toEx + diff > 0 && toId /= s && toId /= t]

    -- Apppend this list into the corresponding buckets
    newNetQ = appendToKthList (appendToKthList cleanQueue fromVertexQueue frH) toVertexQueue toH


{- Appends the provided list to the list at the height `h`. -}
appendToKthList list listToAppend h = result
    where
        -- Get sublist
        sublist = list !! h

        -- Update the sublist
        newSublist = sublist ++ listToAppend

        -- Insert updated item
        result = take h list ++ [newSublist] ++ drop (h + 1) list

{- Lifts the provided vertex and updates the network accordingly. -}
liftVertex :: Network -> VertId -> Int -> Network
liftVertex net@(Network vs es s t bucketQueue) verToLiftId diff = Network updatedVertices es s t bucketQueue
    where 
        -- Get vertices with updated height
        liftedVerts = [ newV | vert@(Vertex vId h ex des) <- vs , 
            let newV = if vId == verToLiftId 
                    then Vertex vId (h + diff) ex des -- Updated vertex
                    else vert ] -- Unchanged vertices

        -- Recompute also descending edges
        updatedVertices = recomputeDescLists liftedVerts es


recomputeDescLists vs es = map updateFn vs
    where
        updateFn (Vertex vId h ex des) = (Vertex vId h ex outDescEdges)
            where
                -- Outcoming edges
                outEdges = getOutEdges es vId

                -- Outcomming DESCENDING edges
                outDescEdges = filterOnlyDescNonSat vs outEdges 0


{- Filteres out edges that are not descending and NOT fully saturated after applying `diff` lift. -}
filterOnlyDescNonSat :: [Vertex] -> [Edge] -> Int -> [(VertId, VertId)]
filterOnlyDescNonSat vs outEdges diff = map (\ (Edge eFr eTo eC eF) -> (eFr, eTo)) (filter (
    \(Edge eFr eTo eC eF) -> 
        let 
            (Vertex _ hFr _ _)  = findVertex vs eFr
            (Vertex _ hTo _ _)  = findVertex vs eTo
        in
            hFr + diff > hTo && eC - eF > 0
    ) outEdges)

{- Removes from and to vertices from active vertex queue. -}
getCleanQueue q frId toId = map (filter (\vId -> vId /= frId && vId /= toId)) q

{-- Gets edge with specified name -}
findVertex vs tarName = head $ filter (\(Vertex name _ _ _) -> name == tarName) vs

{-- Gets edge with specified from/to values -}
findEdge es fr to = head $ filter (\(Edge _fr _to _ _) -> _fr == fr && _to == to) es

{- Gets all OUTCOMING edges for the given vertex. -}
getOutEdges :: [Edge] -> VertId -> [Edge]
getOutEdges es vId = filter (\(Edge eFr _ _ _) -> eFr == vId) es

{- Gets all INCOMING edges for the given vertex. -}
getInEdges :: [Edge] -> VertId -> [Edge]
getInEdges es vId = filter (\(Edge _ eTo _ _) -> eTo == vId) es


{----------------------------------------------
                    Utilies
----------------------------------------------}
getNetworkFlow :: Network -> Rational
getNetworkFlow net@(Network vs es s t bucketQueue) = sum $ map (\(Edge _ _ _ flow) -> flow) (getInEdges es t)

unJust (Just x) = x
unJust Nothing  = error "This shouldn't have happened!"