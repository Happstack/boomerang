{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Text.Boomerang.Pos
    ( Pos
    , InitialPosition(..)
    , ErrorPosition(..)
    , MajorMinorPos(..)
    , incMajor, incMinor
    ) 
    where

import Data.Data (Data, Typeable)

-- | type synonym family that maps an error type to its position type
type family Pos err :: *

-- | extract the position information from an error 
class ErrorPosition err where
    getPosition :: err -> Maybe (Pos err)

-- | the initial position for a position type
class InitialPosition e where
    initialPos :: Maybe e -> Pos e

-- | A basic 2-axis position type (e.g. line, character)
data MajorMinorPos = MajorMinorPos 
    { major :: Integer 
    , minor :: Integer
    }
    deriving (Eq, Ord, Typeable, Data)

-- | increment major position by 'i', reset minor position to 0.. 
-- if you wanted something else.. too bad.
incMajor :: (Integral i) => i -> MajorMinorPos -> MajorMinorPos
incMajor i (MajorMinorPos maj min) = MajorMinorPos (maj + (fromIntegral i)) 0

-- | increment minor position by 'i'
incMinor :: (Integral i) => i -> MajorMinorPos -> MajorMinorPos
incMinor i (MajorMinorPos maj min) = MajorMinorPos maj (min + (fromIntegral i))

instance Show MajorMinorPos where
    show (MajorMinorPos s c) = "(" ++ show s ++ ", " ++ show c ++ ")"
