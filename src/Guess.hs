{-# LANGUAGE OverloadedStrings #-}
module Guess 
    ( validateGuessDecl
    ) where

import Control.Monad.State.Lazy ( evalState, get, put )
import Data.Map ( empty )
import Data.Text ( unpack, Text )
import Shunting (compileExpr, evalRpnExpr)
import Tokenizer ( RunicItem(..), RunicToken(..) )
import CompilerInternal
    ( rStateTransition
    , rSyntaxErrorMsg
    , rUnexpectedErrMsg
    , CtxItem(GuessObj)
    , RStateTransition
    )

validateGuessDecl :: [RunicItem] -> Either String (CtxItem, [RunicItem])
validateGuessDecl tokens = evalState (s0 tokens) ("", "")
    where 
        s0 :: RStateTransition (Text, Text)
        s0 = rStateTransition RGuess (RExpression "") 
            (\lineNo tok rs -> case tok of 
                (RExpression t) -> do
                    put (t, "")
                    s1 rs
                _ -> return (Left $ rUnexpectedErrMsg
                    $ rSyntaxErrorMsg (RExpression "") RGuess (Just $ RunicItem lineNo tok)))

        s1 :: RStateTransition (Text, Text)
        s1 = rStateTransition (RExpression "") RFor (\_ _ rs -> s2 rs)

        s2 :: RStateTransition (Text, Text)
        s2 = rStateTransition RFor (RExpression "") 
            (\_ _ rs -> do
                (name, value) <- get
                return $ do
                    ts <- compileExpr (unpack value) empty
                    numVal <- evalRpnExpr ts empty
                    return (GuessObj name numVal, rs))