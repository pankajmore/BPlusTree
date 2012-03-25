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

type Key = ByteString
type Value = ByteString

newtype BPPtr = BPP Int deriving (Show,Eq)

data BPlusNode = BPLeaf [(Key, Value)] (Maybe BPPtr)
               | BPInternal [(Key, BPPtr)] BPPtr
                 deriving (Show)



