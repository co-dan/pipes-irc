{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Various proxies that help handling the IRC business
module Pipes.Network.IRC.Core where

import           Control.Monad                     (forever)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import qualified Data.Set                          as S
import           Pipes
import           Pipes.Network.TCP
import qualified Pipes.Prelude                     as P

import           Pipes.Network.IRC.Internal.Parser
import           Pipes.Network.IRC.Types


-- | Proxy that parses IRC messages
parseMsg :: (Monad m)
          => Pipe ByteString (Maybe Message) m r
parseMsg = forever $
    await >>= yield . parseComplete messageParser

-- | For debugging purposes
logP :: Pipe ByteString ByteString IO r
logP = forever $ do
 s <- await
 lift . putStrLn $ "Sending response: " ++ Prelude.show s
 yield s

-- | Proxy that filters messages according to 'IRCSettings'
filterMsg :: (MonadIO m, Monad m)
              => IRCSettings
              -> Pipe Message Message m r
filterMsg (IRCSettings{..}) = loop
  where loop = do
          msg <- await
          case msgCommand msg of
            PrivMsgCmd targets txt -> do
              let me = S.insert nick channels
              if (trigger `BS.isPrefixOf` txt)
                 && (not . S.null $ me `S.intersection` targets)
               then yield msg >> loop
               else loop
            _ -> loop

-- | Proxy that responds to PING queries
handlePing :: (MonadIO m, Monad m)
           => Pipe Message Command m r
handlePing = do
  msg <- await
  case msgCommand msg of
    PingCmd a b -> yield (PongCmd a b) >> liftIO (putStrLn "Ping handled") >> handlePing
    _ -> handlePing

-- | Proxy that sends command to the socket
writeMsg :: Socket -> Consumer Command IO r
writeMsg sock = P.map showCommand >-> logP >-> toSocket sock
