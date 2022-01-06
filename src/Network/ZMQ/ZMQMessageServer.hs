{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}

module Network.ZMQ.ZMQMessageServer
  (
    runMsgServer
  )
where

------------------------------------------------------------------------------
import           Network.ConnectionCache
import           Network.ZMQ.NoBlockChan
import           Network.ZMQ.Types
------------------------------------------------------------------------------
import qualified Control.Concurrent                    as CC
import qualified Control.Concurrent.Async              as Async
import qualified Control.Concurrent.Chan.Unagi         as U
import           Control.Monad.State.Strict
import qualified Data.Map.Strict                       as Map
import           Protolude                             hiding (async, newChan,
                                                        readChan, to)
import           System.ZMQ4.Monadic
------------------------------------------------------------------------------

runMsgServer
  :: Channel chanType
  => Proxy   chanType
  -> TransportEnv Address chanType
  -> IO ()
runMsgServer chanType te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    l teLogInfo (here ["started zmqThread"])
    -------------------------
    zmqReceiver <- async $ do
      l teLogInfo (here ["started zmqReceiver (thread)"])
      void (receiver chanType te)
      l teLogErr  (here ["exiting zmqReceiver (thread)"])
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    zmqSender <- async $ do
      l teLogInfo (here ["started zmqSender (thread)"])
      -- The connection cache MUST be created inside the ZMQ monad and cannot be passed outside.
      (_, _, cc) <- mkNewConnections (ConnectionCache Map.empty) teAddrList
      void (sender te cc)
      l teLogErr (here ["exiting zmqSender (thread)"])
    -------------------------
    liftIO $ waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> teLogErr (here ["waitEitherCancel", "zmqReceiver ()"])
      Right v -> teLogErr (here ["waitEitherCancel", "zmqSender", show v])
    -------------------------
    l teLogErr (here ["exiting zmqThread"])

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> teLogErr (here ["died Right ()"])
    Left err -> teLogErr (here ["died Left", show err])
  pure ()
  where
   here t = "ZMQMessageServer":"runMsgServer":show teMyAddr:t
{-# SCC runMsgServer #-}

receiver
  :: Channel chanType
  => Proxy chanType
  -> TransportEnv Address chanType
  -> ZMQ z ()
receiver chanType TransportEnv {..} = do
  sock <- socket Pull
  l teLogInfo (here ["bind"])
  _ <- bind sock teMyAddr
  forever $ do
    newMsg <- receive sock -- GET MSG FROM ZMQ
    l teLogInfo (here ["recv", show newMsg])
    liftIO $ do
      writeC chanType teInboxWrite newMsg -- GIVE MSG TO SYSTEM
      CC.yield
 where
  here t = "ZMQMessageServer":"receiver":show teMyAddr:t
{-# SCC receiver #-}

-- The app layer MUST communicate with ZMQ via the channel
-- because the connection cache must live inside the ZMQ monad.
sender
  :: TransportEnv Address chanType
  -> ConnectionCache Address (Socket z Push)
  -> ZMQ z ()
sender TransportEnv{..} cc0 = do
  ccMvar <- liftIO (newMVar cc0)
  forever $ do
    (OutBoundMsg addrs msg) <- liftIO (U.readChan teOutboxRead) -- GET MSGS FROM SYSTEM
    l teLogInfo (here ["sending to", show addrs, "MSG", show msg])
    cc                      <- liftIO (takeMVar ccMvar)
    (es, nc, cs, cc')       <- getOrMakeConnection cc addrs
    liftIO (putMVar ccMvar cc')
    forM_ es       (\e     -> l teLogErr  (here ["could not connect to", show e]))
    forM_ nc       (\(n,_) -> l teLogInfo (here ["new connection to"   , show n]))
    forM_ (cs++nc) (\(_,s) -> send s [] msg) -- GIVE MSGS TO ZMQ
    l teLogInfo (here ["sent msg"])
 where
  here t = "ZMQMessageServer":"sender":show teMyAddr:t
{-# SCC sender #-}

l :: ([Text] -> IO ())
  ->  [Text]
  -> ZMQ z ()
l f x = liftIO (f x)

------------------------------------------------------------------------------

-- returns ([could not connect],[new connections], [existing connections], cache)
-- TODO: ZMQ viz could not connect
-- TODO: connections are (address, socket) - address for debugging
getOrMakeConnection
  :: ConnectionCache Address (Socket z Push)
  -> [Address]
  -> ZMQ z ( [Address]                  -- cannot connect
           , [(Address, Socket z Push)] -- new
           , [(Address, Socket z Push)] -- existing
           , ConnectionCache Address (Socket z Push)
           )
getOrMakeConnection cc peers  =
  case getConnections cc peers of
    ([],          connections) -> pure ([], [], connections, cc)
    (needConnect, connections) -> do
      (cannotConnect, newConnections, cc') <- mkNewConnections cc needConnect
      pure (cannotConnect, newConnections, connections, cc')
{-# SCC getOrMakeConnection #-}

mkNewConnections
  :: ConnectionCache Address (Socket z Push)
  -> [Address]
  -> ZMQ z ( [Address]                  -- cannot connect
           , [(Address, Socket z Push)] -- new
           , ConnectionCache Address (Socket z Push)
           )
mkNewConnections (ConnectionCache m0) addrs = do
  (absent, present, m) <- foldM go ([], [], m0) addrs
  pure (absent, present, ConnectionCache m)
 where
  go (ab, p, m) address = do
    s <- socket Push
    void (connect s address)
    pure (ab, (address, s):p, Map.insert address s m)
{-# SCC mkNewConnections #-}

