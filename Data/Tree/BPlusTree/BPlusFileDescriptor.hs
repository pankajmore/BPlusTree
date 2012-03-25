module Data.Tree.BPlusTree.BPlusFileDescriptor where

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


readBPlusHeader :: Handle -> IO BPlusFileDescriptor
readBPlusHeader fh = do
    hSeek fh AbsoluteSeek 0
    bs <- B.hGet fh (8 :: Int) -- change 8 to header size
    let bpfd = decode bs :: BPlusFileDescriptor
    return bpfd
      
writeBPlusHeader :: BPlusFileDescriptor -> Handle -> IO ()
writeBPlusHeader bpfd fh = do
    let bs = encode bpfd
    hSeek fh AbsoluteSeek 0
    B.hPut fh bs

newBPFD :: Handle -> Word32 -> Word32 -> IO BPlusFileDescriptor
newBPFD fh bs ri = do
    let bpfd = BPFD bs ri
    writeBPlusHeader bpfd fh
    return bpfd

test = do
    let bpfd = BPFD (512::Word32) (0::Word32)
    print bpfd
    fh <- openFile "test.btree" ReadWriteMode
    writeBPlusHeader bpfd fh
    s <- readBPlusHeader fh
    print s
    print (s == bpfd)
