module Data.BPlusTree.BPlusFileDescriptor where

-- | Structure of B-tree on block storage
-- The full B-tree is stored in a single file
-- A B-tree file has a fixed blocksize, which is always a multiple of
-- 512 bytes. The first block in the file is a header block:

-- Header block:
--  4 bytes - block size
--  4 bytes - index of "root" block
--  What else?
--  All other blocks are node blocks


import Data.Word (Word32)
import Data.Binary
import qualified Data.ByteString.Lazy as B
import System.IO

data BPlusFileDescriptor = BPFD { blockSize :: Word32,
                                  rootIndex :: Word32 }
                           deriving (Show,Eq)

instance Binary BPlusFileDescriptor where
    put bpfd = do 
        put (blockSize bpfd)
        put (rootIndex bpfd)

    get = do
        s <- get :: Get Word32
        i <- get :: Get Word32
        return (BPFD s i)


readBPlusHeader :: FilePath -> IO BPlusFileDescriptor
readBPlusHeader fp = do
    bs <- B.readFile fp
    let bpfd = decode bs :: BPlusFileDescriptor
    return bpfd
      
writeBPlusHeader :: BPlusFileDescriptor -> FilePath -> IO ()
writeBPlusHeader bpfd fp = do
    let bs = encode bpfd
    B.writeFile fp bs

test = do
    let bpfd = BPFD (512::Word32) (0::Word32)
    print bpfd
    writeBPlusHeader bpfd "test.btree"
    s <- readBPlusHeader "test.btree"
    print s
    print (s == bpfd)
