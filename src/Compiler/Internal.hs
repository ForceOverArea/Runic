{-# LANGUAGE Safe #-}
module Compiler.Internal 
    ( CtxItem(..)
    , RnCtx
    , RnNum
    , Runic
    ) where

import Data.Map (Map)
import Text.Parsec (Parsec)

-- | Type alias to parametrize the numeric type for the shunting 
--   yard (in case complex number support is added.)
type RnNum = Double

-- | Type alias for a @Map@ of @String@s to @CtxItem@s to elaborate
--   on what certain identifiers in terms of concrete values.
type RnCtx = Map String CtxItem

-- | Record type for functions, constants, and variables parsed 
--   while compiling Runic source code.
data CtxItem
    = Function Int ([RnNum] -> RnNum)
    | Const RnNum
    | Variable RnNum (Maybe RnNum) (Maybe (RnNum, RnNum))

-- | Alias for the @Parsec@ monad type used in the Runic parser
--   implementation.
type Runic = Parsec String RnCtx