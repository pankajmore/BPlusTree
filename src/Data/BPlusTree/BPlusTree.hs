module BPlusTree where

-- | Implementation of B+ Tree, using a RecordFile as the underlying
-- primitive. A B+ Tree is a BTree where all the actual data is in the
-- leafs.

-- There are two kinds of nodes, leaf nodes and internal nodes. An
-- internal node contains only keys + pointers to other nodes. A leaf
-- node contains only keys + actual data corresponding to each key.

-- An internal node takes the following structure:
--
-- [ <p1> | kA | <p2> | kB | <p3> | kC | <p4> ]
--
-- where, e.g., <p1> points to the place to search for all keys less
-- than or equal to kA, <p2> points to the place to search for all
-- keys > kA and <= kB, and <p4> to the place to search for keys > kC.
--
-- The ps may either point to another internal node, for more searching, or
-- a leaf node.

-- A leaf node takes the structure:
--
-- [ kA | vA | kB | vB | kC | vC | <p> ]
--
-- where the 'vA' is the value corresponding to 'kA' and <p> points to
-- the next leaf node along.

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.BPlusTree.BPlusFileDescriptor
import Data.Binary

type Key = ByteString
type Value = ByteString

type WordSize = Word32

-- The idea is to seek to this location in the file
type BPPtr = Word32 

data BPlusNode = BPLeaf [(Key, Value)] (Maybe BPPtr)
               | BPInternal [(Key, BPPtr)] BPPtr
                 deriving (Show)


-- Probably need to fine tune the serialization of list
instance Binary BPlusNode where
   put (BPLeaf lkv maybeptr) = do
     put (0 :: WordSize) -- initial tag byte to indicate each variant of the data type
     put (maybeptr :: Maybe BPPtr)
     put lkv

   put (BPInternal lkp ptr) = do
     put (1 :: WordSize)
     put (ptr :: BPPtr)
     put lkp

   get = do
     t <- get :: Get WordSize
     case t of
        0 -> do
            maybeptr <- get :: Get (Maybe BPPtr)
            lkv <- get
            return (BPLeaf lkv maybeptr)
        1 -> do
            ptr <- get :: Get (BPPtr)
            lkp <- get
            return (BPInternal lkp ptr)
