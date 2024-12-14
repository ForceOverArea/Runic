{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

{-|
    A module for 
-}
module Compiler.Parser.Grammar
    ( validateDomain
    , validateGuess
    ) where

import Prelude hiding ( lookup )
import Control.Monad.State.Lazy ( lift, StateT )
import Compiler.Parser.Objects ( Token(..) )
import Compiler.Parser.Factory ( (<->), (<+>), liftStashParser, parseExpression, ParserState )
import Data.Map ( insert, lookup, Map )
import Data.Text ( pack, split, unpack, Text )
import Text.Read ( readMaybe )

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
A token validation function for a domain specification in Runic 
source. This validator captures the domain bounds and name for
further processing.
-}
validateDomain :: Token -> [Token] -> Either String (ParserState Token)
validateDomain = parseExpression $ liftStashParser 
    (Expr "") <-> On <-> LBrack <+> Expr "" <-> RBrack

{-|
A token validation function for a guess statement in Runic source. 
This validator captures the guess expression and name for further 
processing.
-}
validateGuess :: Token -> [Token] -> Either String (ParserState Token)
validateGuess = parseExpression $ liftStashParser
    (Expr "") <-> For <+> Expr ""

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
