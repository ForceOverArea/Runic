{-# LANGUAGE Trustworthy #-}
module Parser.Units
    ( convertUnits
    , getUnitConversionData
    , UnitConversionError
    ) where

import safe Control.Monad.Reader (asks, lift)
import Data.Aeson (decodeStrictText)
import safe qualified Data.Map as M (empty, foldlWithKey, insert, lookup, union, Map)
import safe Data.Maybe (fromMaybe)
import safe Data.Text (unpack, Text)
import safe qualified Data.Text.IO as TIO (readFile)
import safe Types (Quantity, RunicT)

-- | An error type for when a unit conversion is attempted and cannot
--   be evaluated due to either not having that unit defined or from
--   trying to convert units that do not make sense (e.g. length to
--   temperature is not a practical conversion.)
data UnitConversionError 
    = UnitNotFound Text
    | UnitQuantityMismatch Text Text
    deriving (Eq, Ord)

-- | Tries to obtain the conversion factor between two given units in 
--   a @RunicT@ monad transformer action. May return a 
--   @UnitConversionError@ if the given units are not defined in 
--   @units.json@ or do not measure the same quantity. 
convertUnits :: Monad m => Text -> Text -> RunicT m (Either UnitConversionError Double)
convertUnits fromUnit toUnit = do
    fromFactor <- tryGetUnitData fromUnit
    toFactor <- tryGetUnitData toUnit
    return $ case (fromFactor, toFactor) of
        (Left fErr, _) -> Left fErr
        (_, Left tErr) -> Left tErr
        (Right (ff, fq), Right (tf, tq)) 
            -> if fq == tq then
                Right $ ff / tf
            else
                Left $ UnitQuantityMismatch fromUnit toUnit

-- | Fetches relevant unit conversion data for units that exist, 
--   invalid units will cause this function to return a 
--   @UnitConversionError@.
tryGetUnitData :: Monad m => Text -> RunicT m (Either UnitConversionError (Double, Quantity))
tryGetUnitData unit = do
    possUnitData <- lift . asks $ M.lookup unit
    return $ maybe (Left $ UnitNotFound unit) Right possUnitData

-- | Flattens the map of quantity names to same-quantity units to a 
--   map of just unit literals to their conversion factors and 
--   @Quantity@ constructor values.
getUnitConversionData :: IO (M.Map Text (Double, Quantity))
getUnitConversionData = do
    rawTree <- getUnitConMap <$> TIO.readFile "units.json"
    return $ M.foldlWithKey f M.empty rawTree
    where 
        f :: M.Map Text (Double, Quantity)
            -> Text 
            -> M.Map Text Double 
            -> M.Map Text (Double, Quantity)
        f m q us = m `M.union` getUnitDataMap q us

-- | Deserializes the text in the @units.json@ file read by the 
--   Runic compiler and throws an error when the structure of 
--   that file is invalid.
getUnitConMap :: Text -> M.Map Text (M.Map Text Double)
getUnitConMap = fromMaybe (error "f ya life. Bing bong!") . decodeStrictText

-- | Converts the map structure stored under each separate quantity 
--   in the @units.json@ file read by the Runic compiler to a more
--   useful structure for quickly checking whether a unit conversion
--   is valid.
getUnitDataMap :: Text -> M.Map Text Double -> M.Map Text (Double, Quantity)
getUnitDataMap q = M.foldlWithKey f M.empty
    where
        f :: M.Map Text (Double, Quantity)
            -> Text
            -> Double
            -> M.Map Text (Double, Quantity)
        f m u v = M.insert u (v, read . unpack $ q) m