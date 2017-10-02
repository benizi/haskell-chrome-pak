{-# LANGUAGE DeriveDataTypeable #-}

module Pak where

import Data.Binary (Binary(..), Get, Word16, Word32)
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as B
import Data.Typeable (Typeable)

data Archive = Archive
  { pakEntries :: [Entry]
  , pakBlob :: B.ByteString
  } deriving (Read, Show)

data Entry = Entry
  { pakEntryId :: Word16
  , pakEntryOffset :: Word32
  , pakEntryLength :: Word32
  } deriving (Read, Show, Typeable)

entryData :: Entry -> B.ByteString -> B.ByteString
entryData Entry { pakEntryOffset = off, pakEntryLength = len } =
  (B.take $ fromIntegral len) . (B.drop $ fromIntegral off)

getIndexEntries :: Integral a => a -> Get [Entry]
getIndexEntries tot = do
  raw <- getIndexEntryList tot []
  return $ map (uncurry fromRaw) $ zip raw $ drop 1 raw
    where
      getIndexEntryList :: Integral a => a -> [(Word16,Word32)] -> Get [(Word16,Word32)]
      getIndexEntryList 0 acc = return $ reverse acc
      getIndexEntryList n acc = do
        eid <- BG.getWord16le
        off <- BG.getWord32le
        getIndexEntryList (n - 1) ((eid,off):acc)
      fromRaw :: (Word16,Word32) -> (Word16,Word32) -> Entry
      fromRaw (eid, a) (_, b) = Entry { pakEntryId = eid
                                      , pakEntryOffset = a
                                      , pakEntryLength = b - a
                                      }

instance Binary Archive where
  put archive = do
    BP.putWord32le 4 -- TODO: vary version
    BP.putWord32le $ fromIntegral $ length $ pakEntries archive
    BP.putWord8 1
  get = do
    BG.skip 4 -- version <- BG.getWord32le
    ct <- BG.getWord32le
    BG.skip 1 -- encoding <- BG.getWord8
    entries <- getIndexEntries ct
    return Archive
      { pakEntries = entries
      , pakBlob = B.empty
      }
