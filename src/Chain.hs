{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chain where

import Protolude hiding (get, put)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Crypto.Hash
import Data.Int (Int64)
import Data.Aeson hiding (json)

import qualified Data.Text as T
import qualified Data.Serialize as S
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
--------------------------------------
-- Types
--------------------------------------
type Index      = Integer
type Hash       = ByteString
type Timestamp  = Integer
type BlockData  = ByteString
type Nonce      = Int64
type BlockChain = [Block]


data Block  = Block 
  { blockIndex   :: Index
  , previousHash :: Hash
  , timestamp    :: Timestamp
  , blockData    :: BlockData
  , nonce        :: Nonce
  , blockHash    :: Hash
  } deriving (Show, Eq, Generic, S.Serialize)

--------------------------------------
-- Hashing
--------------------------------------

sha3_256 :: ByteString -> ByteString
sha3_256 = BA.convert . hashWith SHA3_256

calculateHash 
    ::  Index
    ->  Hash
    ->  Timestamp
    ->  BlockData
    ->  Nonce
    -> ByteString
calculateHash i ph t d n = 
  sha3_256 $ BS.concat [ BB.pack (show i)
                       , ph
                       , BB.pack (show t)
                       , d
                       , BB.pack (show n)  
                       ]

calculateHashForBlock :: Block -> Hash
calculateHashForBlock (Block i ph ts bd n _) = calculateHash i ph ts bd n 


--------------------------------------
-- Build Origin Block
--------------------------------------

currentTime :: IO Integer
currentTime = fmap round getPOSIXTime

originBlock :: Block
originBlock = Block index' previousHash' timestamp' blockData' nonce' blockHash'
  where 
    index' = 0
    previousHash' = "0"
    timestamp' = 0
    blockData' = "<INIT BLOCK DATA>"
    nonce' = 0
    blockHash' = calculateHash index' previousHash' timestamp' blockData' nonce'

--------------------------------------
-- Create a new Block
--------------------------------------

createNewBlock :: Block -> Timestamp -> BlockData -> Block
createNewBlock previousBlock timestamp' blockData' = 
  Block i phash timestamp' blockData' nonce' bhash
    where 
      i = blockIndex previousBlock  + 1
      phash = blockHash previousBlock
      nonce' = proofOfWork i phash timestamp' blockData' 
      bhash = calculateHash i phash timestamp' blockData' nonce'


--------------------------------------
-- Proof Of Work
--------------------------------------

proofOfWork ::  Index -> Hash -> Timestamp -> BlockData -> Nonce
proofOfWork idx prevHash ts bdata' = calcNonce 0  
  where
    dbits = round $ logBase (2 :: Float) $ fromIntegral idx 
    {-dbits = 4-}
    prefix = toS $ replicate dbits '0' 

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        hash' = calculateHash idx prevHash ts bdata' n 
        prefix' = T.take dbits $ encode64 hash' 

isBrokenBlock :: Block -> Block -> Maybe Text
isBrokenBlock prev new 
  | blockIndex prev + 1       /= blockIndex new    = Just "Index is invalid"
  | blockHash prev            /= previousHash new  = Just "PreviousHash is invalid"
  | calculateHashForBlock new /= blockHash new     = Just "Hash is invalid"
  | otherwise                                     = Nothing

isBrokenChain :: BlockChain -> Maybe Text
isBrokenChain (b0:b1:bs) = isBrokenBlock b0 b1 <|> isBrokenChain bs
isBrokenChain [_] = Nothing
isBrokenChain []  = Just "Empty Chain"


emptyBlockchain :: BlockChain
emptyBlockchain = []

addBlock :: Block -> BlockChain -> BlockChain
addBlock _ [] = []
addBlock b (pb:bs) 
  | isNothing (isBrokenBlock pb b) = b : pb : bs
  | otherwise = pb : bs


-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Block where
  toJSON (Block i ph t d n h) =
    object [ "bindex"       .= i
           , "previousHash" .= encode64 ph
           , "timestamp"    .= toJSON t
           , "bdata"        .= encode64 d
           , "nonce"        .= toJSON n
           , "bhash"        .= encode64 h
           ]

instance FromJSON Block where
  parseJSON (Object o) =
    Block <$>  o .: "bindex"
          <*> (o .: "previousHash" >>= decode64)
          <*> (o .: "timestamp"    >>= pure)
          <*> (o .: "bdata"        >>= decode64)
          <*> (o .: "nonce"        >>= pure)
          <*> (o .: "bhash"        >>= decode64)
  parseJSON _ = mzero

encode64 :: ByteString -> Text
encode64 = decodeUtf8 . BS64.encode

decode64 :: (Monad m) => Text -> m ByteString
decode64 = either (panic . toS) pure . BS64.decode . toS
