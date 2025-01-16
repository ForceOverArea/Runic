{-# LANGUAGE Safe #-}
module Compiler.Parser.RunicMath
    (
    ) where

import safe Prelude hiding (const, map)
import safe Compiler.Evaluator (evaluateExprWithCtx)
import safe Compiler.Evaluator.Internal as E (CtxItem(..))
import safe Compiler.Internal as R (CtxItem(..), RnNum, Runic)
import safe Compiler.Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Compiler.Parser.RunicTokens (runicTokenParser)
import safe Data.Map (map)
import safe Text.Parsec (anyChar, char, endOfLine, getState, manyTill)
import safe Text.Parsec.Prim (try)
import safe Text.Parsec.Token (GenTokenParser (comma, identifier, operator, reserved))

expression :: Runic a
expression = do
    expression >> plus >> expression
    -- <|>

plus :: Runic ()
plus = do
    _ <- operator runicTokenParser
    return ()