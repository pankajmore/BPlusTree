module BPlusTree where

-- | Implementation of a B+ Tree

type Key = ByteString
type Value = ByteString

data BPlusNode = BPLeaf [(Key, Value)] (Maybe BPPtr)
               | BPInternal [(Key, BPPtr)] BPPtr
