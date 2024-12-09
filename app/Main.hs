{-# LANGUAGE OverloadedStrings #-}
module Main 
    (main) where

import Control.Monad.State.Lazy ( execStateT )
import Data.Text ( Text )
import Compiler.Parser 
    ( (<?->)
    , (<?+>)
    , liftParser
    , ParserState(..)
    )

-- | 
data Token
    = Keep
    | On
    | LBrack
    | RBrack
    | Expr Text
    deriving (Eq, Show)

validateDomain :: [Token] -> Either String (ParserState Token)
validateDomain stack = execStateT validator (ParserState [] stack)
    where 
        validator = liftParser Keep 
            <?+> (Expr "x") 
            <?-> On
            <?-> LBrack
            <?+> (Expr "0, 100")
            <?-> RBrack

myMain :: Either String (ParserState Token)
myMain = do
    let q = [Keep, Expr "x", {-On,-} LBrack, Expr "0, 100", RBrack]
    parsed <- validateDomain q
    return parsed

main :: IO ()
main = do
    print myMain
