{-# LANGUAGE DeriveDataTypeable #-}

module Pak where

import Control.Monad (forM_)
import Data.Binary (Binary(..), Get, Put, Word16, Word32)
import Data.Binary.Get (getWord16le, getWord32le, getWord8, getByteString)
import Data.Binary.Put (putWord16le, putWord32le, putWord8, putByteString)
import qualified Data.ByteString as S
import Data.Map (Map, fromList, size, toAscList)

type ObjectCount = Word32
type ObjectId = Word16
type Offset = Word32
type ObjectData = S.ByteString

newtype Archive = Archive (Map ObjectId ObjectData)
  deriving (Read, Show)

data IndexEntry = IndexEntry
  { idxId :: ObjectId
  , idxOffset :: Offset
  } deriving (Read, Show)

toEntry :: ObjectId -> Offset -> IndexEntry
toEntry eid off = IndexEntry { idxId = eid, idxOffset = off }

instance Binary Archive where
  put = putArchive
  get = getArchive

putIndexEntry :: IndexEntry -> Put
putIndexEntry IndexEntry { idxId = oid, idxOffset = off } =
  putWord16le oid >> putWord32le off

putArchive :: Archive -> Put
putArchive (Archive m) = do
  putWord32le version
  putWord32le objectCount
  putWord8 encoding
  forM_ indexEntries putIndexEntry
  forM_ values putByteString
    where
      version = 4 -- TODO: handle/verify version(s)
      encoding = 1 -- TODO: handle other encoding(s)
      headerSize = 4 + 4 + 1 -- version + count + encoding
      entrySize = 2 + 4 -- Word16 + Word32 -- id + offset
      objectCount = fromIntegral $ size m
      indexEntryCount = 1 + objectCount
      startOffset = headerSize + indexEntryCount * entrySize
      ordered :: [(ObjectId, ObjectData)]
      ordered = toAscList m
      values = snd <$> ordered
      lengths = fromIntegral . S.length <$> values
      offsets = scanl (+) startOffset lengths
      indexEntries = zipWith toEntry ((fst <$> ordered) ++ [0]) offsets

getIndexEntries :: ObjectCount -> Get [IndexEntry]
getIndexEntries count = do
  getIndexEntryList count []
    where
      getIndexEntryList :: ObjectCount -> [IndexEntry] -> Get [IndexEntry]
      getIndexEntryList 0 acc = return $ reverse acc
      getIndexEntryList n acc = do
        eid <- getWord16le
        off <- getWord32le
        getIndexEntryList (n - 1) (toEntry eid off : acc)

getArchive :: Get Archive
getArchive = do
  _version <- getWord32le
  count <- getWord32le
  _encoding <- getWord8
  entries <- getIndexEntries $ count + 1
  objects <- sequence $ zipWith extractObject entries (drop 1 entries)
  return $ Archive $ fromList objects
    where
      extractObject :: IndexEntry -> IndexEntry -> Get (ObjectId, ObjectData)
      extractObject this next = do
        let IndexEntry { idxId = oid, idxOffset = off } = this
            IndexEntry { idxOffset = end } = next
            len = fromIntegral $ end - off
         in getByteString len >>= return . ((,) oid)
