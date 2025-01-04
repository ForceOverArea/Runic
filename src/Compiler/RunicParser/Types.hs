{-# LANGUAGE Safe #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.RunicParser.Types
    ( toShuntingYdContext
    , tokenMapping
    , RunicContext
    , RunicKeyword(..)
    , RunicObject(..)
    , Token
    , TokenTracker(..)
    ) where

import Compiler.Evaluator.Internal as E ( Context, CtxItem(..), SyNum )
import Data.Map as M ( fromList, map, Map )
import Data.Text ( Text )

{-|
    A class for tokens that should have a different definition of 
    equality than the derivable definition. (e.g. RunicKeyword's 
    Expr value should be identifiable on the basis of whether it is
    an expr.)
-}
class TokenEq a where 
    {-|
        The token equality operator for denoting that the variety of 
        a token is equal to that of another. (e.g. an Expr "a" =+= an
        Expr "b" yields True.)
    -}
    (=+=) :: a -> a -> Bool

{-|
    A type alias for parametrizing what type of numeric value Runic 
    solves for. 
-}
type RunicNum = SyNum

{-|
    A small alphabet representing the keywords in the Runic language.
-}
data RunicKeyword
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

instance TokenEq RunicKeyword where
    (=+=) :: RunicKeyword -> RunicKeyword -> Bool
    (Expr _) =+= (Expr _) = True
    a =+= b = a == b

{-|
    A functor for a token that stores the line number it originated
    from as context.
-}
data TokenTracker t = TokenTracker Int t
    deriving (Eq, Ord)

instance TokenEq t => TokenEq (TokenTracker t) where
    (=+=) :: TokenTracker t -> TokenTracker t -> Bool
    (TokenTracker _ tokenA) =+= (TokenTracker _ tokenB)
        = tokenA =+= tokenB

instance Show t => Show (TokenTracker t) where
    show :: TokenTracker t -> String
    show (TokenTracker lineNo x)
        = show x ++ "(line " ++ show lineNo ++ ")"

instance Functor TokenTracker where
    fmap :: (a -> b) -> TokenTracker a -> TokenTracker b
    fmap f (TokenTracker lineNo x)
        = TokenTracker lineNo (f x)

{-|
    A value tracked by the global (or local in the case of functions)
    context of the Runic compiler. Can be a variable with a domain and 
    initial guess, a constant value, or a pure numerical function.
-}
data RunicObject
    -- | Value, Guess, Min, Max
    = RnVariable RunicNum RunicNum RunicNum RunicNum
    -- | Value
    | RnConst RunicNum
    -- | # Args, Function
    | RnFunction Int ([RunicNum] -> RunicNum)

{-|
    A type alias for a Map Text RunicObject aka the context at the 
    local and global levels used by the Runic compiler.
-}
type RunicContext = Map Text RunicObject

{-|
    Converts the context managed by Runic to the less 
    information-dense context used by its shunting yard evaluator 
    implementation.
-}
toShuntingYdContext :: RunicContext -> Context
toShuntingYdContext = M.map toCtxItem
    where
        toCtxItem :: RunicObject -> CtxItem
        toCtxItem (RnVariable value _ _ _) = E.Const value
        toCtxItem (RnConst value) = E.Const value
        toCtxItem (RnFunction x y) = E.Function x y

{-|
    Represents a runic keyword wrapped in a TokenTracker functor
-}
type Token = TokenTracker RunicKeyword

{-|
    Provides a mapping between keywords in valid Runic syntax and 
    their corresponding Token value.
-}
tokenMapping :: Map Text RunicKeyword
tokenMapping = fromList
    [ ("keep"   , Keep)
    , ("on"     , On)
    , ("["      , LBrack)
    , ("]"      , RBrack)
    , ("guess"  , Guess)
    , ("for"    , For)
    , ("const"  , Compiler.RunicParser.Types.Const)
    , ("fn"     , Compiler.RunicParser.Types.Function)
    , ("return" , Return)
    , ("if"     , If)
    , ("then"   , Then)
    , ("else"   , Else)
    , ("system" , System)
    , ("end"    , End)
    ]
    {-  Expr does not have a corresponding keyword. It corresponds to 
        expressions that will be parsed for building context for 
        solving the system later. -}