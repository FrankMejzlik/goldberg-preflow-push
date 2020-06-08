
module Goldberg where

{-

The main source file for highest-label push-relabel (Goldberg) algorithm.

IMPLEMENTATION:
    The algorithm is run by the function `runGoldberg` and is divided into two main phases:
        1) `goldbergInitialize` - Intializes the heights and sends the first preflow (wave) from the SOURCE vertex.
        2) `goldbergStep` - One iteration of the algorithm that is repeated while there are some ACTIVE VERTICES.
            
            This is then divided into two main sub-functions that are called based on state of the currently
            selected active vertex (always the one with the highest height):

            a) `push` - Pushes the maximum flow using the current excess through the provided edge.
            b) `liftVertex` - Increases the height of the vertex and updates the descending edges for each vertex.

    Many of the mentioned functions also call some helper functions, but those above mentioned are the key functions
    to look at when inspecting the algorithm. These above mentioned also correspond to how the pseudocode of 
    the Golberg algorithm is structured.


DATA TYPES (defined in `types.hs`):
    V - Vertex as taken from the user.
    E - Edge as taken from the user.
    N - Network as taken (and also returned to) from the user.

    VertId - Identifier for vertices.
    Vertex - Vertex with additional meta-data essential for the algorithm.
    Edge - Edge with additional meta-data for essential the algorithm.
    Network - Flow network with additional meta-data essential for the algorithm.
-}

-- Third party libs
import Text.Pretty.Simple -- USAGE: stack ghci --package pretty-simple ./main.hs
import Data.Maybe

-- My modules
import Types

{-  The MAIN algorithm function - computes and returns the final flow network with the
    maximum flow (in form of flow values in it). -}
runGoldberg :: N -> N
runGoldberg net = toUserNetwork $ until goldbergShouldTerminate goldbergStep initNet
    where   
        -- Convert and initialize the algorithm
        initNet = goldbergInitialize net


{- Returns True if the Goldberg algorithm is finished, False otherwise. -}
goldbergShouldTerminate :: Network -> Bool
goldbergShouldTerminate net@(Network vs es s t bucketQueue) 
    | isNothing $ getHighestVert bucketQueue = True
    | otherwise = False


{- Initializes the user provided network for the Goldberg algorithm -}
goldbergInitialize :: N -> Network 
goldbergInitialize net@(N vs es s t) = foldr (flip push) (toComputeNetwork net) (getRawOutEdges es s)


{- Converts compute network type back to the user types. 
    This throws out any mata-data uninteresting to the user. -}
toUserNetwork :: Network -> N
toUserNetwork (Network vs es s t q) = N oldVs oldEs s t
    where
        -- Convert vertices
        oldVs = map fv vs

        -- Convert and filter out the helper reverse edges
        oldEs = filter (\(E _ _ eC _) -> eC /= 0) (map fe es)

        -- Conversion functions
        fv (Vertex vId _ _ _) = V vId
        fe (Edge eFr eTo eC eF) = E eFr eTo eC eF


{- Converts user network to representation for the algorithm calculations -}
toComputeNetwork :: N -> Network
toComputeNetwork (N vs es s t) = Network vss ess s t (replicate (length vs * 2 + 1) [])
  where
    vss = map fv vs -- Edgetended vertices
    ess = map fe es  ++ map feRev es -- Convert edges and add their reverses
    fv v@(V name)
        | name == s = Vertex name (length vs) capSum (map (\(E fr to _ _) -> (fr, to)) (filter (\ (E fr _ _ _) -> fr == name) es)  )
        | name == t = Vertex name 0 0.0 []
        | otherwise = Vertex name 0 0.0 []
    
    fe (E fr to c _) = Edge fr to c 0.0
    feRev (E fr to c _) = Edge to fr 0.0 0.0

    capSum = sum (map (\(E _ _ c _) -> c) es) -- Sum of all edge capacities

{- One iteration of the algorithm. Final value is the updated network. -}
goldbergStep :: Network -> Network
goldbergStep net@(Network vs es s t netQ) = if not (null descEdges) -- If no pushable edge found
    then push net (head descEdges) -- We can push => push 
    else liftVertex net vertId 1 -- Cannot push => lift by 1
        where
            (Vertex _ _ _ descEdges) = findVertex vs vertId
            vertId = unJust $ getHighestVert netQ


{- Returns the highest vertext from the bucket queue. -}
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
    ( foundEdge@(Edge eFr eTo eCap eFlow), foundEdgeRev@(Edge erFr erTo erCap erFlow) ) = findUnsaturatedEdge es fr to
    restEdges = filter (\(Edge _fr _to _ _) -> not ( (_fr == fr || _fr == to) && (_to == fr || _to == to) ) ) es
    
    -- New edges
    newEdge = Edge eFr eTo eCap (eFlow + diff)
    newEdgeRev = Edge erFr erTo erCap ((-eFlow) - diff)

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

{- Recomputes descending edges list for every vertex. -}
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
findUnsaturatedEdge es fr to = (mainEdge, revEdge)
    where
        -- Able to be pushed through
        mainEdge@(Edge _fr _to _c _f) = head $ filter (\(Edge _fr _to _c _f) -> _fr == fr && _to == to && _c - _f > 0) es
        
        -- It's reverse partner - it is uniquely determined by exactly inverse flow value
        revEdge = head $ filter (\(Edge _rfr _rto _rc _rf) -> _rto == fr && _rfr == to && _rf == (-_f) ) es

{- Gets all OUTCOMING edges for the given vertex. -}
getOutEdges :: [Edge] -> VertId -> [Edge]
getOutEdges es vId = filter (\(Edge eFr _ _ _) -> eFr == vId) es

{- Gets all INCOMING edges for the given vertex. -}
getInEdges :: [Edge] -> VertId -> [Edge]
getInEdges es vId = filter (\(Edge _ eTo _ _) -> eTo == vId) es

{- Gets all INCOMING RAW edges for the given vertex. -}
getInRawEdges :: [E] -> VertId -> [E]
getInRawEdges es vId = filter (\(E _ eTo _ _) -> eTo == vId) es

{- Gets all outcoming edges from the list of User Edges (data E)/ -}
getRawOutEdges :: [E] -> Int -> [(VertId, VertId)]
getRawOutEdges es from = map (\(E fr to _ _) -> (fr, to)) (filter (\(E fr to _ _) -> fr == from) es)

{----------------------------------------------
                    Utilies
----------------------------------------------}

{- Returns the N-th iteration of the algorithm. -}
runGoldbergIterN net n = toUserNetwork $ iterate goldbergStep initNet !! n
    where   
        -- Convert and initialize the algorithm
        initNet = goldbergInitialize net

{- Pretty prints the flow in the network -}
pPrintFlow :: N -> IO ()
pPrintFlow (N vs es s t) =
    do
        mapM_ print es

{- Computes current flow of the provided network. -}
getNetworkFlow :: N -> Rational
getNetworkFlow net@(N vs es s t) = sum $ map (\(E _ _ _ flow) -> flow) (getInRawEdges es t)

{- Finds the specified edge (and it's reverse partner). -}
findEdge es fr to = (mainEdge, revEdge)
    where
        -- Able to be pushed through
        mainEdge@(Edge _fr _to _c _f) = head $ filter (\(Edge _fr _to _c _f) -> _fr == fr && _to == to) es
        
        -- It's reverse partner - it is uniquely determined by exactly inverse flow value
        revEdge = head $ filter (\(Edge _rfr _rto _rc _rf) -> _rto == fr && _rfr == to && _rf == (-_f) ) es

{- Unjusts the provided argument. -}
unJust (Just x) = x
unJust Nothing  = error "This shouldn't have happened!"