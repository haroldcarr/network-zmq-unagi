{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Strict            #-}

module Network.ZMQ.Init where

------------------------------------------------------------------------------
import           Network.ZMQ.NoBlockChan
import           Network.ZMQ.Types
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi as U
import           Protolude
------------------------------------------------------------------------------

initialize
  :: forall  chanType addr
   . Channel chanType
  => Proxy   chanType
  -> addr              -- listen address
  -> ([Text] -> IO ()) -- logErr
  -> ([Text] -> IO ()) -- logInfo
  -> IO ( TransportEnv addr chanType
        , ReadChanType chanType
        , CHANNEL_WRITE (OutBoundMsg addr) )
initialize chanType me le li = do
  (inboxW , inboxR)  <- mkC chanType
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv me [] inboxW outboxR le li
       , inboxR
       , outboxW )
{-# SCC initialize #-}
