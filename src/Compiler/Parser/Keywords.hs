{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Keywords
    ( getText
    , keywords
    , makeRt
    , tokenMapping
    , Token(..)
    , RToken
    , RunicToken(..)
    ) where

import Compiler.Parser.Factory ( tEqual, TokenEq )
import Data.Map ( fromList, keys, Map )
import Data.Text ( pack, Text )

{-| 
Represents a token value expected by the Runic Compiler's main
parser state machine.
-}
data Token 
    = Keep
    | On
    | LBrack
    | RBrack
    | Guess
    | For
    | Const
    | Function
    | Return
    | If
    | Then
    | Else
    | System
    | End
    | NewLine
    | Expr Text
    deriving (Eq, Ord, Show)

-- | Equality definition for the Token type
instance TokenEq Token where
    tEqual :: Token -> Token -> Bool
    Expr _ `tEqual` Expr _ = True
    x `tEqual` y = show x == show y -- Works since only Expr can contain instance-specific text

-- | Extracts the text from an `Expr` Token. 
getText :: Token -> Text
getText (Expr t) = t
getText tok = pack $ show tok

-- | A token with line number it originated from
data RunicToken a = RunicToken Int a
    deriving (Eq, Ord)

-- | A type alias for a RunicToken containing a Token value
type RToken = RunicToken Token

instance Functor RunicToken where
    fmap :: (a -> b) -> RunicToken a -> RunicToken b
    fmap f (RunicToken ln rt) = RunicToken ln $ f rt

instance TokenEq a => TokenEq (RunicToken a) where
    tEqual :: RunicToken a -> RunicToken a -> Bool
    (RunicToken _ x) `tEqual` (RunicToken _ y) = x `tEqual` y

instance Show a => Show (RunicToken a) where
    show :: RunicToken a -> String
    show (RunicToken ln tok) = show tok ++ "(on line " ++ show ln ++ ")"

makeRt :: a -> RunicToken a
makeRt = RunicToken 0

{-|
Provides a mapping between keywords in valid Runic syntax and their 
corresponding Token value.
-}
tokenMapping :: Map Text Token
tokenMapping = fromList
    [ ("keep"   , Keep)
    , ("on"     , On)
    , ("["      , LBrack)
    , ("]"      , RBrack)
    , ("guess"  , Guess)
    , ("for"    , For)
    , ("const"  , Const)
    , ("fn"     , Function)
    , ("return" , Return)
    , ("if"     , If)
    , ("then"   , Then)
    , ("else"   , Else)
    , ("system" , System)
    , ("end"    , End)
    ] 
    -- Expr does not have a corresponding keyword. It corresponds to 
    -- expressions that will be parsed for building context for 
    -- solving the system later.

{-|
A list of only the keywords in valid Runic syntax generated from
the text-Token mapping.
-}
keywords :: [Text]
keywords = keys tokenMapping
