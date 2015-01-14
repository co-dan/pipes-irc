{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Proxy.IRC (
  defSettings, runIrc,
  module Control.Proxy.IRC.Types,
  module Control.Proxy.IRC.Util,
  module Control.Proxy.IRC.Run,
  module Control.Proxy.IRC.Proxies
  ) where


import Prelude hiding (drop)

import Pipes
import Pipes.Network.TCP
import Data.ByteString (ByteString, drop)
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
   { channels     = S.fromList ["#bot-test2"]
   , checkPrivMsg = False
   , nick         = "test-bot22"
   , host         = "orwell.freenode.net"
   , port         = "6667"
   , trigger      = "> "
   , hook         = myHook
   }
                    

myHook :: MsgHook
myHook IRCSettings{..} = fix $ \loop -> do
  msg <- await
  case msgCommand msg of
    PrivMsgCmd targets txt -> do
      let m = drop 2 txt   
      yield $ PrivMsgCmd (nick `S.delete` targets) m
      loop
    _ -> loop

