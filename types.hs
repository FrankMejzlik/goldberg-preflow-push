module Types where

{- 
    Data types for networks that the user will input and also get back.

    No extra meta-data included.
-}

-- Vertex
data V =
    V
        { v_name :: Int
        } -- Vertex <ID>
    deriving (Eq, Show)

-- Edge
data E =
    E
        { e_from :: Int
        , e_to :: Int
        , e_cap :: Rational
        , e_flow :: Rational
        } -- Edge { <from_ID>, <to_ID>, <capacity> }
    deriving (Eq, Show)

-- Network
data N =
    N [V] [E] Int Int -- FlowNetwork { <vertices>, <edges> }
    deriving (Eq, Show)

{- 
    Algorithm meta-types used that are extended with some metadata.

    E.g. descending edges lists, bucket queue of active vertices, etc.
-}
-- For IDs of vertices
type VertId = Int

-- Vertex holding extra list of descending edges
-- (Vertex name h ex des)
data Vertex =
    Vertex
        { vertex_name :: VertId
        , vertex_h :: Int
        , vertex_ex :: Rational
        , vertex_des :: [(VertId, VertId)]
        }
    deriving (Eq, Show)

-- Edgetended edge holding flow going through it
-- (Edge eFr eTo eC eF)
data Edge =
    Edge
        { edge_fr :: Int
        , edge_to :: Int
        , eedge_c :: Rational
        , edge_f :: Rational
        }
    deriving (Eq, Show)

-- Network structure for algorithm computation with extra meta data
-- (Network vs es s t q)
data Network =
    Network
        { network_vs :: [Vertex]
        , network_es :: [Edge]
        , network_s :: Int
        , network_t :: Int
        , network_Q :: [[VertId]]
        }
    deriving (Eq, Show)
