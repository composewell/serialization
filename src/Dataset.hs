{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Dataset(carsData,irisData) where
import           Control.DeepSeq
-- import qualified Data.Binary                as B
-- import Codec.Serialise as CBOR
-- import qualified Data.Flat                  as F
-- import qualified Data.Serialize             as C
-- import qualified Data.Persist                as R
import qualified Data.Store                 as S
import           Numeric.Datasets           (getDataset)
-- import           Numeric.Datasets.Abalone   (abalone)
import           Numeric.Datasets.Car
import           Numeric.Datasets.Iris

import qualified Streamly.Data.Serialize as SS
import qualified Data.Store.TH as STH

instance NFData RelScore
-- instance B.Binary RelScore
-- instance C.Serialize RelScore
-- instance CBOR.Serialise RelScore
-- instance F.Flat RelScore
instance S.Store RelScore
--  $(STH.makeStore ''RelScore)
$(SS.deriveSerialize ''RelScore)
-- instance R.Persist RelScore

instance NFData RelSize
-- instance B.Binary RelSize
-- instance C.Serialize RelSize
-- instance CBOR.Serialise RelSize
-- instance F.Flat RelSize
instance S.Store RelSize
--  $(STH.makeStore ''RelSize)
$(SS.deriveSerialize ''RelSize)
-- instance R.Persist RelSize

instance NFData Acceptability
-- instance B.Binary Acceptability
-- instance C.Serialize Acceptability
-- instance CBOR.Serialise Acceptability
-- instance F.Flat Acceptability
instance S.Store Acceptability
--  $(STH.makeStore ''Acceptability)
$(SS.deriveSerialize ''Acceptability)
-- instance R.Persist Acceptability

instance NFData Count
-- instance B.Binary Count
-- instance C.Serialize Count
-- instance CBOR.Serialise Count
-- instance F.Flat Count
instance S.Store Count
--  $(STH.makeStore ''Count)
$(SS.deriveSerialize ''Count)
-- instance R.Persist Count

deriving instance Eq Car
instance NFData Car
-- instance B.Binary Car
-- instance C.Serialize Car
-- instance CBOR.Serialise Car
-- instance F.Flat Car
instance S.Store Car
--  $(STH.makeStore ''Car)
$(SS.deriveSerialize ''Car)
-- instance R.Persist Car

instance NFData IrisClass
-- instance B.Binary IrisClass
-- instance C.Serialize IrisClass
-- instance CBOR.Serialise IrisClass
-- instance F.Flat IrisClass
instance S.Store IrisClass
--  $(STH.makeStore ''IrisClass)
$(SS.deriveSerialize ''IrisClass)
-- instance R.Persist IrisClass

deriving instance Eq Iris
instance NFData Iris
-- instance B.Binary Iris
-- instance C.Serialize Iris
-- instance CBOR.Serialise Iris
-- instance F.Flat Iris
instance S.Store Iris
--  $(STH.makeStore ''Iris)
$(SS.deriveSerialize ''Iris)
-- instance R.Persist Iris

-- irisData = iris
irisData :: [Iris]
irisData = by 500 iris

carsData :: IO [Car]
carsData = by 20 <$> getDataset car
-- carsData = getDataset car
-- abaloneData = by 10 <$> getDataset abalone

by :: Int -> [a] -> [a]
by n = concat . replicate n

-- test :: IO ()
-- test = do
--    -- The Iris data set is embedded
--    print (length iris)
--    print (head iris)
--    cars <- getDataset car
--    print (length cars)
--    print (head cars)
--    -- print $ F.flat cars
--    -- The Abalone dataset is fetched
--    abas <- getDataset abalone
--    print (length abas)
--    print (head abas)
