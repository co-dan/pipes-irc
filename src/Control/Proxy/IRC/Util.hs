{-# LANGUAGE OverloadedStrings #-}
module Control.Proxy.IRC.Util where

import Data.ByteString (ByteString)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F  
import Data.Monoid ((<>))
  
sayTargets :: (Foldable t) => ByteString -> t ByteString -> ByteString
sayTargets m = F.foldMap (\t -> "PRIVMSG " <> t <> " :" <> m <> "\n")

               
