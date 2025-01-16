{-# LANGUAGE Safe #-}
{-# LANGUAGE InstanceSigs #-}
module Compiler.Internal
    ( CtxItem(..)
    , RnCtx
    , RnNum
    , Runic
    ) where

import safe qualified Data.Map as Map (Map)
import safe Text.Parsec (Parsec)

-- | Type alias to parametrize the numeric type for the shunting 
--   yard (in case complex number support is added.)
type RnNum = Double

-- | Type alias for a @Map@ of @String@s to @CtxItem@s to elaborate
--   on what certain identifiers in terms of concrete values.
type RnCtx = Map.Map String CtxItem

-- | Record type for functions, constants, and variables parsed 
--   while compiling Runic source code.
data CtxItem
    = Function Int ([RnNum] -> RnNum)
    | Const RnNum
    | Variable RnNum (Maybe RnNum) (Maybe (RnNum, RnNum))

instance Show CtxItem where
    show :: CtxItem -> String
    show (Function n _) = "Double" ++ concat (replicate n " -> Double")
    show (Const n) = show n
    show (Variable v g d) = show v
        ++ "(guess: " ++ maybe "N/A" show g ++ "), "
        ++ maybe "(domain: [-inf, inf])" showDmn d
        where
            showDmn :: (Double, Double) -> String
            showDmn (mn, mx) = "[" ++ show mn ++ "," ++ show mx ++ "]"

-- | Alias for the @Parsec@ monad type used in the Runic parser
--   implementation.
type Runic = Parsec String RnCtx