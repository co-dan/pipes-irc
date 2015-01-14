{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Proxy.IRC.Run where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as S
import Pipes  
import qualified Pipes.Prelude as P
import Pipes.Network.TCP
import Data.Monoid
import qualified Data.Foldable as F
import Data.Maybe (isJust, fromJust)  
  
import Control.Proxy.IRC.Types hiding (HostName)
import Control.Proxy.IRC.Proxies  


runIrc :: IRCSettings -> IO ()
runIrc (settings@IRCSettings{..}) = do
 putStrLn "connecting.."
 connect host port $ \(sock,addr) -> do
  putStrLn $ "Connected to " ++ show addr
  S.send sock $ mconcat ["NICK ", nick, "\n",
                         "USER ", nick, " bot bot: ", nick, "\n\n"]
  F.forM_ channels $ \c -> S.send sock ("JOIN " <> c <> "\n")
  runEffect $ forever $
    for (forkedMessagesS sock) $ \msg -> do      
      yield msg >-> handlePing >-> writeMsg sock
      yield msg >-> filterMsg settings
                >-> hook settings
                >-> writeMsg sock

forkedMessagesS sock =
   fromSocket sock 512
   >-> parseMsg
   >-> P.filter isJust --- ugly!!
   >-> P.map fromJust



{-
             /---->p1-->p2-->
Source ---->/                \----> Sink
            \                /
             \---->p3------->

Source :: Producer A m r
p1 :: Pipe A B m r
p2 :: Pipe B C m r
p3 :: Pipe A C m r
Sink :: Consumer C m r 
-}
