{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Pipes.Network.IRC (
  defSettings, runIrc,
  module Pipes.Network.IRC.Types,
  module Pipes.Network.IRC.Util,
  module Pipes.Network.IRC.Run,
  module Pipes.Network.IRC.Core
  ) where


import           Prelude                 hiding (drop)

import           Data.ByteString         (drop)
import           Data.Function           (fix)
import qualified Data.Set                as S

import           Pipes
import           Pipes.Network.IRC.Core
import           Pipes.Network.IRC.Run
import           Pipes.Network.IRC.Types
import           Pipes.Network.IRC.Util

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

