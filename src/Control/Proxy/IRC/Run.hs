{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Proxy.IRC.Run where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as S
import Control.Proxy  
import Control.Proxy.TCP
import Data.Monoid
import qualified Data.Foldable as F
import Data.Maybe (isJust, fromJust)  
  
import Control.Proxy.IRC.Types hiding (HostName)
import Control.Proxy.IRC.Proxies  


runIrc :: IRCSettings -> IO ()
runIrc (settings@IRCSettings{..}) = connect host port $ \(sock,addr) -> do
  putStrLn $ "Connected to " ++ show addr
  S.send sock $ mconcat ["NICK ", nick, "\n",
                         "USER ", nick, " bot bot: ", nick, "\n\n"]
  F.forM_ channels $ \c -> S.send sock ("JOIN " <> c <> "\n")
  forever $ runProxy $
    handlePings sock
    >-> filterMsgD settings
    >-> hook settings
    >-> writeMsgD sock


handlePings :: (MonadTrans (p3 C () () Message), Proxy p3, MonadIO (p3 C () () Message IO)) => Socket -> () -> Producer p3 Message IO ()
handlePings sock =
  runProxyK $
    forkedMessagesS sock
    >-> handlePingD
    >-> hoist lift . (writeMsgD sock)

forkedMessagesS sock =
  (runProxyK $ hoist_P liftIO .
   (socketReadS 512 sock
    >-> parseMsgD
    >-> filterD (isJust)
    >-> mapD (fromJust)
    >-> printD) >-> fork)
    

