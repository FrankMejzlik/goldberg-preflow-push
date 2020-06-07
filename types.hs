module Types where

{- 
    User input data types 
-}
-- Vertex
data V = 
    V { id_::Int }   -- Vertex <ID>
        deriving (Eq, Show)

-- Edge
data E = 
    E { fr_::Int, to_::Int, c_::Double }   -- Edge { <from_ID>, <to_ID>, <capacity> }
        deriving (Eq, Show)

-- Network
data N = 
    N [V] [E] Int Int         -- FlowNetwork { <vertices>, <edges> }
        deriving (Eq, Show)

{- 
    Algorithm meta-types used 
-}
type VertId = Int

-- Vertex holding extra list of descending edges
-- (Vertex name h ex des)
data Vertex = 
    Vertex  { vertex_name::VertId,     vertex_h::Int, vertex_ex::Double, vertex_des::[(VertId, VertId)] }
        deriving (Eq, Show)

-- Edgetended edge holding flow going through it
data Edge = 
    Edge { edge_fr::Int, edge_to::Int, eedge_c::Double, edge_f::Double } 
        deriving (Eq, Show)

-- Network structure for algorithm computation with extra meta data
data Network = 
    Network { network_vs::[Vertex],  network_es::[Edge], network_s::Int, network_t::Int, network_Q::[[VertId]] }
        deriving (Eq, Show)

