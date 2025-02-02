{-# LANGUAGE Safe #-}
module Data.Runic.Types
    ( CtxItem(..)
    , Quantity(..)
    , RnCtx
    , RnNum
    , RunicT
    , UCMap
    ) where

import safe Control.Monad.Reader (ReaderT)
import safe qualified Data.Map as M (Map)
import safe Data.Text (Text)
import safe Text.Parsec (ParsecT)

-- | Type alias to parametrize the numeric type for the shunting 
--   yard (in case complex number support is added.)
type RnNum = Double

-- | Type alias for a @Map@ of @String@s to @CtxItem@s to elaborate
--   on what certain identifiers in terms of concrete values.
type RnCtx = M.Map String CtxItem

-- | Record type for functions, constants, and variables parsed 
--   while compiling Runic source code.
data CtxItem
    = Function Int ([RnNum] -> RnNum)
    | Const RnNum
    | Variable RnNum (Maybe RnNum) (Maybe (RnNum, RnNum))

instance Show CtxItem where
    show (Function n _) = "Double" ++ concat (replicate n " -> Double")
    show (Const v) = show v
    show (Variable v g d) = show v
        ++ "(guess: " ++ maybe "N/A" show g ++ "), "
        ++ maybe "(domain: [-inf, inf])" showDmn d
        where
            showDmn :: (Double, Double) -> String
            showDmn (mn, mx) = "[" ++ show mn ++ "," ++ show mx ++ "]"

-- | A type alias for the map of units to their respecitve conversion
--   factors and type of quantity measured.
type UCMap = M.Map Text (Double, Quantity)

-- | An enum for the various types of quantities whose units may be
--   converted in Runic syntax.
data Quantity
    = Length
    | Angles
    | Area
    | Volume
    | Velocity
    | VolumetricFlow
    | Frequency
    | Time
    | Mass
    | Moles
    | Force
    | Energy
    | Power
    | Temperature
    | TempDifference
    | Pressure
    | DynamicViscosity
    | KinematicViscosity
    | Charge
    | ElectromotiveForce
    | Current
    | ElectricalResistance
    | Capacitance
    | Inductance
    | DipoleMoment
    | MagneticFlux
    | MagneticFluxDensity
    | MagneticFieldStrength
    | Illuminance
    | IlluminanceFlux
    | NonDimensional
    deriving (Eq, Ord, Read, Show)

-- | Alias for the @Parsec@ monad type used in the Runic parser
--   implementation.
type RunicT m = ParsecT String RnCtx (ReaderT UCMap m)