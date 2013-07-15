{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Various proxies that help handling the IRC business
module Control.Proxy.IRC.Proxies where

import Control.Proxy
import Control.Proxy.TCP
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS  
import Data.Function (fix)
import qualified Data.Set as S
import Network.Socket (Socket)

import Control.Proxy.IRC.Internal.Parser
import Control.Proxy.IRC.Types

-- | Fork the output of a proxy
-- 
-- Example usage:
-- 
-- @
-- messagesS = runProxyK $ hoist_P liftIO . socketReadS 512 sock >-> fork
-- handlePings = runProxyK $ messagesS >-> handlePingD >-> sendData
-- handleMessages = runProxyK $ handlePings >-> handleMessageD >-> ...
-- @
-- 
fork :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
     => () -> Consumer p1 a (Producer p2 a (Producer p3 a m)) r
fork () =
  runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
    a <- request ()          -- Request output from the 'Consumer'
    lift $ respond a         -- Send output to the outer 'Producer'
    lift $ lift $ respond a  -- Send output to the inner 'Producer'

-- | Proxy that parses IRC messages
parseMsgD :: (Proxy p, Monad m)
           => x -> p x ByteString x (Maybe Message) m r
parseMsgD = runIdentityK . foreverK $
            request >=> respond . parseComplete messageParser

            
-- | For debugging purposes
logD :: (Proxy p) => x -> p x ByteString x ByteString IO r
logD = useD (\s -> putStrLn $ "Sending response: " ++ show s)
            
-- | Proxy that filters messages according to 'IRCSettings'
filterMsgD :: (Proxy p, Monad m)
               => IRCSettings
               -> () -> p () Message () Message m r
filterMsgD (IRCSettings{..}) () = runIdentityP loop
  where loop = do
          msg <- request ()
          case msgCommand msg of
            PrivMsgCmd targets txt -> do
              let me = S.insert nick channels
              if ((trigger `BS.isPrefixOf` txt)
                  && (not . S.null $ me `S.intersection` targets))
               then respond msg >> loop
               else loop
            _ -> loop

-- | Proxy that responds to PING queries
handlePingD :: (Proxy p, Monad m)
            => () -> p () Message () Command m r
handlePingD () = runIdentityP $ fix $ \f -> do
  msg <- request ()
  case msgCommand msg of
    PingCmd a b -> respond (PongCmd a b) >> f
    _ -> f
  
-- | Proxy that sends command to the socket
writeMsgD :: (Proxy p) => Socket -> a -> p a Command a ByteString IO r
writeMsgD sock = mapD showCommand >-> logD >-> socketWriteD sock
