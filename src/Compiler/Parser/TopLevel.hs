

module Compiler.Parser.TopLevel 
    (
    ) where

{-| 
A type alias for a map of names (Text) to their corresponding 
Runic-relevant data.
-}   
type CtxMap = Map Text CtxItem

{-| 
An item that should be stashed as part of the context to a system
of equations.
-}   
data CtxItem 
    = CtxGuessDmn Double Double Double
    | CtxConst Double
    -- Todo: add this functionality later
    -- | CtxFunction Text (Int -> [Double] -> Double)
    deriving (Eq, Show)

{-|
Tries to add an item to the given global context state, returning an
error message if the given name collides with an existing item.
-}
tryAddName :: CtxMap -> Text -> CtxItem -> Either String CtxMap
tryAddName ctx name newItem = case lookup name ctx of
    Just oldItem -> Left $ itemNameCollisionError name oldItem
    Nothing -> Right $ insert name newItem ctx

{-|
A template for the error message reported for each kind of `CtxItem`
that may be found in the global context while parsing the Token 
stream.
-} 
itemNameCollisionError :: Text -> CtxItem -> String
itemNameCollisionError name item = case item of
    CtxGuessDmn guess mn mx -> "name collision: " 
        ++ unpack name
        ++ " is a guess/domain hint with a guess of " 
        ++ show guess
        ++ " and a domain of [" 
        ++ show mn 
        ++ "," 
        ++ show mx 
        ++ "]"
    CtxConst val -> "name collision: '"
        ++ unpack name
        ++ "' is a constant with a value of "
        ++ show val           