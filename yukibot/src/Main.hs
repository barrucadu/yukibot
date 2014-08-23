{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (void)
import Data.Aeson          (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), object, eitherDecode)
import Data.Aeson.Encode.Pretty (Config(..), encodePretty')
import Data.Monoid         ((<>))
import Data.Text           (Text, pack)
import Network             (HostName)
import Network.IRC.Asakura
import Network.IRC.Asakura.State (snapshot, rollback)
import Network.IRC.Asakura.Types
import Network.IRC.IDTE
import System.Directory    (doesFileExist)

import qualified Data.ByteString.Lazy            as B
import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Permissions as P

data YukiState = YukiState
    { _commandState    :: C.CommandStateSnapshot
    , _permissionState :: P.PermissionStateSnapshot
    }

instance ToJSON YukiState where
    toJSON ys = object [ "prefixes"    .= toJSON (_commandState    ys)
                       , "permissions" .= toJSON (_permissionState ys)
                       ]

instance FromJSON YukiState where
    parseJSON (Object v) = YukiState
                             <$> v .: "prefixes"
                             <*> v .: "permissions"
    parseJSON _ = fail "Expected object"

main :: IO ()
main = do
  confExists <- doesFileExist "yukibot.json"
  if confExists
  then runWithStateFrom "yukibot.json"
  else runWithFreshState

runWithStateFrom :: String -> IO ()
runWithStateFrom fn = do
  json <- B.readFile fn
  case eitherDecode json of
    Right ys -> runWithState $ Just ys
    Left _   -> putStrLn "Failed to parse configuration file"

runWithFreshState :: IO ()
runWithFreshState = runWithState Nothing

runWithState :: Maybe YukiState -> IO ()
runWithState ys = do
  cconf <- connect "irc.freenode.net" 6667
  state <- newBotState
  (permissionState, commandState) <- initState

  C.registerCommand commandState "echo"  Nothing            bounceBack
  C.registerCommand commandState "rizon" (Just P.God)       joinRizon
  C.registerCommand commandState "join"  (Just $ P.Admin 0) joinChannel
  C.registerCommand commandState "part"  (Just $ P.Admin 0) partChannel
  C.registerCommand commandState "quit"  (Just P.God)     quitNetwork

  addGlobalEventHandler' state $ C.eventRunner commandState

  case cconf of
    Right cconf' -> do
      void $ run cconf' (defaultIRCConf "yukibot") state
      css <- snapshot commandState
      pss <- snapshot permissionState
      let ys = YukiState { _commandState    = css
                         , _permissionState = pss
                         }

      let bs = encodePretty' (Config 4 compare) ys
      B.writeFile "yukibot.json" bs
    Left err     -> putStrLn err

  where initState = case ys of
                      Just ys' -> do
                        permissionState <- rollback $ _permissionState ys'
                        commandState    <- rollback $ _commandState ys'
                        return (permissionState, commandState permissionState)

                      Nothing -> do
                        permissionState <- P.initialise
                        commandState    <- C.initialise "#" permissionState

                        P.setNetworkPermission permissionState "barrucadu" "irc.freenode.net" P.God

                        return (permissionState, commandState)


bounceBack :: [Text] -> IRCState -> Event -> Bot (IRC ())
bounceBack _ _ ev = case _source ev of
                      Channel n c -> return . send $ privmsg c $ n <> ": " <> msg
                      User    n   -> return . send $ query n msg
                      _           -> return $ return ()
    where msg = pack . show . _message $ ev

joinRizon :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinRizon _ _ _ = do
  cconf <- connect "irc.rizon.net" 6667
  case cconf of
    Right cconf' -> addNetwork cconf' $ defaultIRCConf "yukibot"
    Left err -> error err
  return $ return ()

joinChannel :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinChannel (chan:_) _ _ = return . send $ join chan

partChannel :: [Text] -> IRCState -> Event -> Bot (IRC ())
partChannel _ _ ev = case _source ev of
                       Channel _ c -> return $ leaveChannel c $ Just "Banished by magic"
                       _ -> return $ return ()

quitNetwork :: [Text] -> IRCState -> Event -> Bot (IRC ())
quitNetwork _ _ _ = return $ do
  send $ quit (Just "I must now die, in order that the JSON be produced. Farewell.")
  disconnect
