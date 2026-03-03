module Hw2 where
import Parser 

type Tag = String
type Attr = (String,String)
data XMLNode = XMLNode 
    { nodeTag      :: Tag
    , nodeAttrs    :: [Attr]
    , nodeChildren :: [XMLNode]
    , nodeText     :: Maybe String
    , nodeId       :: String 
    } 
    deriving (Show, Eq)