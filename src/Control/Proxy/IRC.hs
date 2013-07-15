{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Proxy.IRC (
  defSettings, runIrc,
  module Control.Proxy.IRC.Types,
  module Control.Proxy.IRC.Util,
  module Control.Proxy.IRC.Run,
  module Control.Proxy.IRC.Proxies
  ) where

import Control.Proxy
import Control.Proxy.TCP
import Data.ByteString (ByteString)
import Data.Function (fix)
import qualified Data.Set as S
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as S

import Control.Proxy.IRC.Proxies  
import Control.Proxy.IRC.Run
import Control.Proxy.IRC.Types 
import Control.Proxy.IRC.Util

defSettings :: IRCSettings
defSettings = IRCSettings
   { channels     = S.fromList ["#idia-test"]
   , checkPrivMsg = False
   , nick         = "iDiagrams"
   , host         = "irc.freenode.net"
   , port         = "6667"
   , trigger      = "> "
   , hook         = myHook
   }
                    


myHook :: MsgHook
myHook IRCSettings{..} () = runIdentityP $ fix $ \loop -> do
  msg <- request ()
  case msgCommand msg of
    PrivMsgCmd targets txt -> do
      respond $ PrivMsgCmd (nick `S.delete` targets) txt
      loop
    _ -> loop


mkServer :: (Proxy p)
         => Socket
         -> ByteString -> Server p ByteString ByteString IO ()
mkServer sock = socketReadS 4095 sock >-> unitU >-> useU (S.send sock)



