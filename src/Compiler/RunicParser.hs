{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.RunicParser
    ( topLevelStateMachine
    ) where

import Control.Monad ( when )
import Control.Monad.Except ( throwError )
import Data.Text ( split, Text )
import Compiler.Evaluator ( evaluateExprWithCtx, SyNum )
import Compiler.RunicParser.Internal
    ( (.>)
    , addToContext
    , buildRegex
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


-- | The top level process of the Runic parser. This function calls 
--	 other RunicTopLevel actions
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
    halves <- runParser $ buildRegex (Expr "") .> On .> Expr ""
    let name = head halves -- safe because list will be 2 items long if regex succeeds
    let rawText = last halves
    possTok <- tryGetFromCtx name
    bounds <- computeDomain rawText
    case possTok of
        Nothing -> addToContext name $ RnVariable 1.0 Nothing (Just bounds)
        Just (RnVariable v g Nothing) -> addToContext name $ RnVariable v g (Just bounds)
        _ -> throwError errMsg
    where
        errMsg :: String
        errMsg = "declared a domain expression for a variable that \
            \already has a domain declared"

-- | Takes the text within from a Runic domain expression and
--   evaluates it to a tuple of numeric values to add to the local ctx
--   for later use by the solver.
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

-- | Handles the parsing of a guess declaration in Runic syntax
parseGuessExpression :: RunicTopLevel ()
parseGuessExpression = do
    halves <- runParser $ buildRegex (Expr "") .> For .> Expr ""
    let name = head halves -- safe because list will be 2 items long if regex succeeds
    let rawText = last halves
    possTok <- tryGetFromCtx name
    ctx <- toShuntingYdContext <$> getContext
    guess <- case evaluateExprWithCtx rawText ctx of 
        (Right guessValue) -> return guessValue
        (Left errMsg) -> throwError errMsg
    case possTok of
        Nothing -> addToContext name $ RnVariable 1.0 (Just guess) Nothing
        Just (RnVariable v Nothing d) -> addToContext name $ RnVariable v (Just guess) d
        _ -> throwError "declared a domain expression for a\
            \ variable that already has a domain declared"

-- | Handles the parsing of a constant value declaration in Runic syntax
parseConstExpression :: RunicTopLevel ()
parseConstExpression = do
    rawText <- runParser $ buildRegex Const .> Expr ""
    (name, expr) <- parseRawConstExpr $ head rawText
    possTok <- tryGetFromCtx name
    ctx <- toShuntingYdContext <$> getContext
    value <- case evaluateExprWithCtx expr ctx of
        (Right value) -> return value
        (Left errMsg) -> throwError errMsg
    case possTok of
        Nothing -> addToContext name $ RnConst value
        _ -> throwError 
            $ "constant value collides with existing name: " 
            ++ show name

-- | Splits a constant declaration into its name and value (expression)
parseRawConstExpr :: Text -> RunicTopLevel (Text, Text)
parseRawConstExpr rawText = do
    let halves = split (== '=') rawText
    if length halves == 2 then
        return (head halves, last halves)
    else
        throwError $ "expected '=' in const expression. found: "
            ++ show rawText

-- | Handles the parsing of function 
parseFunctionExpression :: RunicTopLevel ()
parseFunctionExpression = do
    rawText <- runParser $ buildRegex Function .> Expr "" .> NewLine 
    throwError "ligma balls" -- this is where we need the function substate built :/
