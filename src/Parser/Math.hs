{-# LANGUAGE Safe #-}
module Parser.Math
    ( conversion
    , conversion'
    ) where

import safe Prelude hiding (const, map)
import safe Data.Text (pack, Text)
import safe Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Parser.Lang (runicTokenParser)
import safe Parser.Units (convertUnits)
import safe Text.Parsec (anyChar, char, endOfLine, getState, manyTill)
import safe Text.Parsec.Prim (try)
import safe Text.Parsec.Token (GenTokenParser (..))
import safe Types (CtxItem(..), RnNum, RunicT)

unit :: RunicT m Text
unit = do
    letters

-- | A unit conversion specified in the format [from->to] where
--   'from' and 'to' are units of the same measured quantity to be
--   converted. 
conversion :: RunicT m RnNum
conversion = brackets runicTokenParser conversionFactor

-- | Shorthand for the internals of a @conversion@ lexeme.
conversionFactor :: RunicT m RnNum
conversionFactor = do
    fromUnit <- unit
    reservedOp runicTokenParser "->"
    toUnit <- unit
    possCf <- convertUnits fromUnit toUnit
    case possCf of
        Right cf -> return cf
        Left _ -> error "ligma balls"

-- | A unit conversion specified in EES syntax rather than the Runic
--   syntax-sugar notation.
conversion' :: RunicT m RnNum 
conversion' = do
    reserved runicTokenParser "convert"
    units <- parens runicTokenParser 
        $ commaSep runicTokenParser 
        $ stringLiteral runicTokenParser
    case fmap pack units of 
        [fromUnit, toUnit] -> do 
            possCf <- convertUnits fromUnit toUnit
            case possCf of
                Right cf -> return cf
                Left _ -> error "ligma balls"
        _ -> error "ligma balls"

-- | 
expression :: RunicT m a
expression = do
    expression >> plus >> expression
    -- <|>

plus :: RunicT m ()
plus = do
    _ <- reservedOp runicTokenParser "+"
    return ()