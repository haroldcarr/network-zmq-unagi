{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.ZMQ.Types where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Data.Serialize.Text                      ()
import qualified Prelude
import           Protolude
------------------------------------------------------------------------------

type CHANNEL_WRITE a
  = U.InChan  a

type CHANNEL_READ  a
  = U.OutChan a

type Address = Prelude.String -- because that is what ZMQ4 uses

data OutBoundMsg addr = OutBoundMsg
  { obmTo   :: [addr]
  , obmBody :: ByteString
  } deriving (Eq, Generic)

data BlockOrNonBlock = BlockingChannel | NonBlockingChannel

type family WriteChanType a where
  WriteChanType 'BlockingChannel    = CHANNEL_WRITE ByteString
  WriteChanType 'NonBlockingChannel = UNB.InChan    ByteString

type family ReadChanType  a where
  ReadChanType 'BlockingChannel     =     CHANNEL_READ ByteString
  ReadChanType 'NonBlockingChannel  = MVar (UNB.Stream ByteString)

type family WriteReadChanType a where
  WriteReadChanType a = (WriteChanType a, ReadChanType a)

data TransportEnv addr bornb = TransportEnv
  { teMyAddr     :: addr                    -- listen address
  , teAddrList   :: [addr]                  -- peers known at initialization
  , teInboxWrite :: WriteChanType bornb
  , teOutboxRead :: CHANNEL_READ (OutBoundMsg addr) -- outbound blocking channel
  , teLogErr     :: [Text] -> IO ()
  , teLogInfo    :: [Text] -> IO ()
  }

