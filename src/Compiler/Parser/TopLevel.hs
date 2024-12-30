

module Compiler.Parser.TopLevel 
    (
    ) where

import Compiler.Evaluator.Internal ( Context, CtxItem(..) )
import Data.Text ( Text )

{-|
Tries to add an item to the given global context state, returning an
error message if the given name collides with an existing item.
-}
tryAddName :: Context -> Text -> CtxItem -> Either String Context
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
    Function n f -> "name collision: " 
        ++ show name
        ++ " is a function with the signature: " 
        ++ show guess
        ++ " and a domain of [" 
        ++ show mn 
        ++ "," 
        ++ show mx 
        ++ "]"
    Const val -> "name collision: '"
        ++ unpack name
        ++ "' is a constant with a value of "
        ++ show val           