{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

------------------------------------------------------------------------------
import           Network.ZMQ.Init
import           Network.ZMQ.NoBlockChan
import           Network.ZMQ.Types
import           Network.ZMQ.ZMQMessageServer
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi as U
import           Data.Proxy
import qualified Data.Serialize                as S
import qualified Prelude
import           Protolude                     hiding (to)
import qualified Data.Text                     as T
------------------------------------------------------------------------------

main :: IO ()
main  = getArgs >>= \case
  ("b" :i:_) -> example (Proxy :: Proxy 'BlockingChannel)    (Prelude.read i)
  ("nb":i:_) -> example (Proxy :: Proxy 'NonBlockingChannel) (Prelude.read i)
  _          -> example (Proxy :: Proxy 'NonBlockingChannel) 25000

newtype RPC = RPC Int deriving (Eq, Generic, Show)
instance S.Serialize RPC

example :: forall chanType. Channel chanType => Proxy chanType -> Int -> IO ()
example chanType limit = do
  let a            = "tcp://127.0.0.1:10000"
      b            = "tcp://127.0.0.1:10001"
      le adr m     = print (T.pack adr:m)
      li adr m     = print (T.pack adr:m)
  (at, ainr, aobw) <- initialize chanType a (le a) (li a)
  (bt, binr, bobw) <- initialize chanType b (le a) (li a)
  runMsgServer chanType at
  runMsgServer chanType bt
  void $ runConcurrently $ (,,,)
    <$> Concurrently (sendMsgs b aobw)
    <*> Concurrently (sendMsgs a bobw)
    <*> Concurrently (recvMsgs ainr)
    <*> Concurrently (recvMsgs binr)
 where
  sendMsgs to c =
    forM_ [1::Int .. limit] $ \i ->
      U.writeChan c (OutBoundMsg [to] (S.encode (RPC i)))

  recvMsgs inr = do
    ms <- readC chanType inr 2000
    for_ ms doDecode
    recvMsgs inr

  doDecode m = case S.decode m of
    Left err ->
      print ["failed S.decode"::Text, show m, show err]
    Right rpc@(RPC i) ->
      when (i == limit) $ do
        print (["receive", "decoded", show rpc]::[Text])
        exitSuccess
