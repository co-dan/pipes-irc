{-# LANGUAGE OverloadedStrings #-}
-- | Taken from the @fastirc@ library
-- (c) Ertugrul Söylemez
module Control.Proxy.IRC.Internal.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.Char (toUpper)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as C8
  

import Control.Proxy.IRC.Types

-- | Run a parser completely.
parseComplete :: Parser a -> C8.ByteString -> Maybe a
parseComplete p = complete . parse p
  where
    complete :: Result a -> Maybe a
    complete (Partial f)  = complete (f C8.empty)
    complete (Done _ r)   = Just r
    complete (Fail _ _ _) = Nothing


-- | Parser for IRC messages.
messageParser :: Parser Message
messageParser =
  Message <$> option Nothing (Just <$> try userSpec)
          <*> commandParser
  where
    userSpec :: Parser User
    userSpec = char ':' *> userParser <* skipMany1 (char ' ')

  
commandParser :: Parser Command
commandParser =
  try numCmd <|>
  stringCmd
  where
    cmdArg :: Parser CommandArg
    cmdArg = do
      skipMany1 (char ' ')
      try lastArg <|> takeWhile1 isIRCTokChar

      where
        lastArg :: Parser CommandArg
        lastArg = char ':' *> P.takeWhile isMessageChar

    commaArg :: Parser (Set CommandArg)
    commaArg = S.filter (not . C8.null) . S.fromList . C8.split ',' <$> cmdArg

    intArg :: Parser (Maybe Int)
    intArg = option Nothing (fmap fst . C8.readInt <$> cmdArg)

    joinCmd :: Parser Command
    joinCmd = do
      channels <- C8.split ',' <$> cmdArg
      keys <- option [] $ C8.split ',' <$> cmdArg
      many cmdArg
      return . JoinCmd . M.fromList $
        zip channels (map Just keys ++ repeat Nothing)

    numCmd :: Parser Command
    numCmd = NumericCmd <$> decimal <*> many cmdArg

    optArg :: Parser (Maybe CommandArg)
    optArg = option Nothing (Just <$> cmdArg)

    stringCmd :: Parser Command
    stringCmd = do
      cmd <- C8.map toUpper <$> takeWhile1 isCommandChar
      case cmd of
        "JOIN" -> joinCmd
        "KICK" -> KickCmd <$> commaArg <*> commaArg <*> optArg <* many cmdArg
        "MODE" ->
          try ((\a b c -> ModeCmd (Just (a,b,c)))
               <$> cmdArg
               <*> cmdArg
               <*> many cmdArg)
          <|> (many cmdArg >>= guard . null >> pure (ModeCmd Nothing))
        "NICK" -> NickCmd <$> cmdArg <*> intArg <* many cmdArg
        "NOTICE" -> NoticeCmd <$> commaArg <*> cmdArg <* many cmdArg
        "PART" -> PartCmd <$> commaArg <*> optArg <* many cmdArg
        "PASS" -> PassCmd <$> cmdArg <* many cmdArg
        "PING" -> PingCmd <$> cmdArg <*> optArg <* many cmdArg
        "PONG" -> PongCmd <$> cmdArg <*> optArg <* many cmdArg
        "PRIVMSG" -> PrivMsgCmd <$> commaArg <*> cmdArg <* many cmdArg
        "QUIT" -> QuitCmd <$> optArg <* many cmdArg
        "TOPIC" -> TopicCmd <$> cmdArg <*> optArg <* many cmdArg
        "USER" -> UserCmd <$> cmdArg <*> cmdArg <*> cmdArg <*> cmdArg <* many cmdArg
        _      -> StringCmd cmd <$> many cmdArg
  

userParser :: Parser User
userParser =
  try full <|> nickOnly
  where
    full :: Parser User
    full =
      User <$> P.takeWhile1 isNickChar <* char '!'
           <*> P.takeWhile1 isUserChar <* char '@'
           <*> P.takeWhile1 isHostChar

    nickOnly :: Parser User
    nickOnly = Nick <$> P.takeWhile1 isNickChar


-- | Character predicate for channel names.
isChannelChar :: Char -> Bool
isChannelChar c = isIRCTokChar c && c /= ','


-- | Character predicate for channel passwords.
isChanPwdChar :: Char -> Bool
isChanPwdChar = isChannelChar


-- | Character predicate for IRC commands.
isCommandChar :: Char -> Bool
isCommandChar = inClass "A-Za-z0-9_"


-- | Character predicate for IRC user hostnames.  In the string @x!y\@z@
-- the substring @z@ is the user's hostname.
isHostChar :: Char -> Bool
isHostChar = isUserSpecChar


-- | Character predicate for IRC end of line characters.
isIRCEOLChar :: Char -> Bool
isIRCEOLChar c = c == '\n' || c == '\r'


-- | Character predicate for IRC tokens.
isIRCTokChar :: Char -> Bool
isIRCTokChar c = c /= ' ' && c /= '\r' && c /= '\n'


-- | Character predicate for IRC messages.
isMessageChar :: Char -> Bool
isMessageChar c = c /= '\n' && c /= '\r'


-- | Character predicate for IRC nicknames.  This function considers
-- high bytes (0x80 to 0xFF) and most nonstandard ASCII bytes as valid,
-- because most modern IRC daemons allow nonstandard nicknames.
isNickChar :: Char -> Bool
isNickChar = isUserSpecChar


-- | Character predicate for IRC servers.
isServerChar :: Char -> Bool
isServerChar c = inClass "a-zA-Z0-9.:-" c || c >= '\x80'


-- | Character predicate for IRC usernames.  In the string @x!y\@z@ the
-- substring @y@ is the user's username.
isUserChar :: Char -> Bool
isUserChar = isUserSpecChar


-- | Character predicate for nicknames, usernames and hostnames.
isUserSpecChar :: Char -> Bool
isUserSpecChar c = c > '!' && c /= '@'
