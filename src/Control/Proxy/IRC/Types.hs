{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | A lot of this code is taken from the @fastirc@ library
-- by Ertugrul Söylemez
module Control.Proxy.IRC.Types where

import Control.Proxy
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Network.Socket (ServiceName)
import qualified Network.Socket
import Text.Printf


data IRCSettings = IRCSettings
   { channels     :: !(Set ChannelName) -- ^ Channels to join and listen on
   , checkPrivMsg :: !Bool              -- ^ /Not supporteed yet/
   , nick         :: !NickName
   , host         :: !Network.Socket.HostName
   , port         :: !ServiceName
   , hook         :: MsgHook
   , trigger      :: !ByteString
   }

type MsgHook = (Proxy p, MonadIO m, Monad m)
               => IRCSettings
               -> () -> Pipe p Message Command m r
   
type ChannelKey  = ByteString
type ChannelName = ByteString
type CommandArg  = ByteString
type CommandName = ByteString
type HostName    = ByteString
type NickName    = ByteString
type ServerName  = ByteString
type TargetName  = ByteString
type UserName    = ByteString
                   
data User = Nick NickName
          | User NickName UserName HostName            
          deriving (Eq)
                   
instance Show User where
  show (Nick n) = C8.unpack (n <> ":_@_")
  show (User n u h) = C8.unpack $ mconcat [n, ":", u, "@", h]


data Command
  = StringCmd CommandName [CommandArg]  -- ^ Arbitrary string command.
  | NumericCmd Integer [CommandArg]     -- ^ Arbitrary numeric command.

  | JoinCmd     (Map ChannelName (Maybe ChannelKey))
  | KickCmd     (Set ChannelName) (Set NickName) (Maybe CommandArg)
  | ModeCmd     (Maybe (TargetName, CommandArg, [CommandArg]))
  | NickCmd     NickName (Maybe Int)
  | NoticeCmd   (Set TargetName) CommandArg
  | PartCmd     (Set ChannelName) (Maybe CommandArg)
  | PassCmd     CommandArg
  | PingCmd     ServerName (Maybe ServerName)
  | PongCmd     ServerName (Maybe ServerName)
  | PrivMsgCmd  (Set TargetName) CommandArg
  | QuitCmd     (Maybe CommandArg)
  | TopicCmd    ChannelName (Maybe CommandArg)
  | UserCmd     UserName CommandArg CommandArg CommandArg
  deriving (Eq, Read, Show)
           
isPingCmd :: Command -> Bool
isPingCmd (PingCmd _ _) = True
isPingCmd _           = False
                        
data Message =
  Message {
    msgOrigin  :: !(Maybe User), -- ^ Message origin (user/server).
    msgCommand :: !Command           -- ^ Message command or numeric.
  } deriving (Eq, Show)

             
showMessage :: Message -> ByteString
showMessage (Message origin cmd) =
  case origin of
    Nothing -> showCommand cmd
    Just o  ->
      C8.append (':' `C8.cons` showUserSpec o)
                (' ' `C8.cons` showCommand cmd)

showCommand :: Command -> ByteString
showCommand = (`C8.snoc` '\n') . showCommand'
              
showCommand' :: Command -> ByteString
showCommand' cmd =
  case cmd of
    StringCmd cmdStr args  -> C8.append cmdStr (showArgs args)
    NumericCmd cmdNum args ->
      C8.append (C8.pack . printf "%03i" $ cmdNum)
               (showArgs args)

    JoinCmd channels ->
      case formatJoins channels of
        (chanList, "")      -> "JOIN" +-+ [chanList]
        (chanList, keyList) -> "JOIN" +-+ [chanList, keyList]
    KickCmd channels nicks Nothing ->
      "KICK" +-+ [commaList channels, commaList nicks]
    KickCmd channels nicks (Just reason) ->
      "KICK" +-+ [commaList channels, commaList nicks, reason]
    ModeCmd Nothing          -> "MODE"
    ModeCmd (Just (target, mode, args)) ->
      "MODE" +-+ [target, mode] ++ args
    NickCmd nick (Just hc)   -> "NICK" +-+ [nick, C8.pack (show hc)]
    NickCmd nick Nothing     -> "NICK" +-+ [nick]
    NoticeCmd targets text   -> "NOTICE" +-+ [commaList targets, text]
    PartCmd chans Nothing    -> "PART" +-+ [commaList chans]
    PartCmd chans (Just reason) ->
      "PART" +-+ [commaList chans, reason]
    PassCmd pwd              -> "PASS" +-+ [pwd]
    PingCmd srv1 Nothing     -> "PING" +-+ [srv1]
    PingCmd srv1 (Just srv2) -> "PING" +-+ [srv1, srv2]
    PongCmd srv1 Nothing     -> "PONG" +-+ [srv1]
    PongCmd srv1 (Just srv2) -> "PONG" +-+ [srv1, srv2]
    PrivMsgCmd targets text  -> "PRIVMSG" +-+ [commaList targets, text]
    QuitCmd Nothing          -> "QUIT" +-+ []
    QuitCmd (Just reason)    -> "QUIT" +-+ [reason]
    TopicCmd channel Nothing -> "TOPIC" +-+ [channel]
    TopicCmd channel (Just newTopic) ->
      "TOPIC" +-+ [channel, newTopic]
    UserCmd user vhost vport realName ->
      "USER" +-+ [user, vhost, vport, realName]

  where
    (+-+) :: C8.ByteString -> [C8.ByteString] -> C8.ByteString
    cmd +-+ args = C8.append cmd (showArgs args)
    infix 4 +-+

    formatJoins :: Map ChannelName (Maybe ChannelKey) ->
                   (CommandArg, CommandArg)
    formatJoins channels = (chanList, keyList)
      where
        (withKey, withoutKey) = M.partition isJust channels
        chanWithKeyAssocs = M.assocs withKey
        chanList = C8.intercalate "," $ map fst chanWithKeyAssocs ++
                                       M.keys withoutKey
        keyList  = C8.intercalate "," $ map (fromJust . snd) chanWithKeyAssocs

    commaList :: Set CommandArg -> CommandArg
    commaList = C8.intercalate "," . S.toList

    showArgs :: [CommandArg] -> ByteString
    showArgs [] = C8.empty
    showArgs [arg]
      | C8.null arg        = " :"
      | C8.head arg == ':' = C8.append " :" arg
      | C8.elem ' ' arg    = C8.append " :" arg
      | otherwise         = C8.cons ' ' arg
    showArgs (arg:args) =
      C8.append (C8.cons ' ' arg) (showArgs args)


showUserSpec :: User -> ByteString
showUserSpec (Nick n) = n
showUserSpec (User n u h) = C8.concat [ n, C8.cons '!' u, C8.cons '@' h ]
