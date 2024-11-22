module Utils 
    ( eitherFromMaybe
    ) where

eitherFromMaybe :: String -> Maybe a -> Either String a
eitherFromMaybe _ (Just y) = Right y
eitherFromMaybe errMsg Nothing = Left errMsg