{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.RunicParser
    ( topLevelStateMachine
    ) where

import Control.Monad ( when )
import Control.Monad.Except ( catchError, throwError )
import Data.Text ( split, Text )
import Compiler.Evaluator ( evaluateExprWithCtx, SyNum )
import Compiler.RunicParser.Internal
    ( (<.>)
    , addToContext
    , buildRegex
    , cycleToken
    , getContext
    , tryGetFromCtx
    , runParser
    , tryGetToken
    , RunicTopLevel
    )
import Compiler.RunicParser.Types 
    ( getToken
    , RunicKeyword(..)
    , RunicObject(..)
    , toShuntingYdContext 
    )


{-|

-}
topLevelStateMachine :: RunicTopLevel ()
topLevelStateMachine = do
    possTok <- tryGetToken
    case possTok of
        Nothing -> return () -- exit the process
        Just tracked -> do
            let tok = getToken tracked
            case tok of
                Keep -> parseDomainExpression
                Guess -> parseGuessExpression
                Const -> parseConstExpression
                Function -> parseFunctionExpression
                invalidToken -> throwError $ "expected either 'keep', \
                    \'guess', 'const', or 'fn' keyword to start a \
                    \phrase, but found " ++ show invalidToken
            topLevelStateMachine
            -- cycleToken -- TODO: is this handled by each branch?

-- | Handles the parsing of a domain declaration in Runic syntax
parseDomainExpression :: RunicTopLevel ()
parseDomainExpression = do
    name:rawText:_ <- runParser $ buildRegex 
        (Expr "") <.> On <.> LBrack <.> Expr "" <.> RBrack
    possTok <- tryGetFromCtx name
    bounds <- computeDomain rawText
    catchError
    case possTok of
        Nothing -> 
            addToContext name $ RnVariable 1.0 Nothing (Just bounds)
        Just (RnVariable v g Nothing) -> 
            addToContext name $ RnVariable v g (Just bounds)
        _ -> throwError errMsg
    where
        errMsg :: String
        errMsg = "declared a domain expression for a variable that \
            \already has a domain declared"

-- | Takes the text within from a Runic domain expression and
-- evaluates it to a tuple of numeric values to add to the local ctx
-- for later use by the solver.
computeDomain :: Text -> RunicTopLevel (SyNum, SyNum)
computeDomain domainExpr = do
    let rawBounds = split (== ',')  domainExpr
    let nBounds = length rawBounds
    syCtx <- toShuntingYdContext <$> getContext
    when (nBounds /= 2) 
        $ throwError 
        $ "expected 2 comma-delimited values but found " 
        ++ show nBounds
        ++ ". function calls with multiple arguments can not be made \
        \ in a domain expression"
    let domain = mapM (`evaluateExprWithCtx` syCtx) rawBounds
    case domain of
        (Right bounds) -> return (head bounds, last bounds)
        (Left errMsg) -> throwError errMsg
