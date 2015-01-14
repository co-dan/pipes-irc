{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Pipes.Network.IRC.Run where

import           Control.Monad             (forever)
import qualified Data.Foldable             as F
import           Data.Maybe                (fromJust, isJust)
import           Data.Monoid
import qualified Network.Socket.ByteString as S
import           Pipes
import           Pipes.Network.TCP
import qualified Pipes.Prelude             as P

import           Pipes.Network.IRC.Core
import           Pipes.Network.IRC.Types   hiding (HostName)


runIrc :: IRCSettings -> IO ()
runIrc (settings@IRCSettings{..}) =
 connect host port $ \(sock,_) -> do
  S.send sock $ mconcat ["NICK ", nick, "\n",
                         "USER ", nick, " bot bot: ", nick, "\n\n"]
  F.forM_ channels $ \c -> S.send sock ("JOIN " <> c <> "\n")
  runEffect $ forever $
    for (parsedMessages sock) $ \msg -> do
      yield msg >-> handlePing >-> writeMsg sock
      yield msg >-> filterMsg settings
                >-> hook settings
                >-> writeMsg sock

parsedMessages sock =
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
