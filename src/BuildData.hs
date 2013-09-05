module BuildData (
        Build, BuildState, 
        getBuildData, getBuildLog, getBuildDiff, 
        state, package, branch, started, finished
      ) where

import Data.List
import Network.Curl.Download
import Network.Curl.Opts
import Text.JSON
import Text.JSON.Types
import qualified Data.ByteString.Char8 as BS

data BuildState = Success | Tested | InProgress | Start | Error | None | Unknown deriving (Show)

data Build = Build 
  {
    package  :: String,
    state    :: BuildState,
    branch   :: String,
    started  :: String,
    finished :: String    
  } deriving (Show)

instance JSON Build where
  showJSON = undefined
  readJSON (JSObject o) = return Build {
          state    = grabState o,
          package  = grab o "name",
          branch   = grab o "branch",
          started  = grab o "started",
          finished = grab o "finished"}


grabState object = 
  case grab object "state" of
    "success"     -> Success
    "tested"      -> Tested
    "in_progress" -> InProgress
    "start"       -> Start
    "error"       -> BuildData.Error
    "none"        -> None
    a             -> error $ "Unknown state " ++ a
grab object field = 
  case get_field object field of
    Nothing            -> error $ "Invalid field " ++ show field
    Just (JSString s') -> fromJSString s' 


parseJSON :: String -> [(String, Build)]
parseJSON jsonString = 
  case decode jsonString of 
    Ok (JSObject a) -> reverse $ sortBy cmp $ map innerParse $ fromJSObject a
    _ -> []
  where
    innerParse (build, json) = 
      (build, 
      case readJSON json of 
        Ok a -> a 
        _    -> error $ "Invalid data for " ++ build 
      )
    cmp (_, Build{started = a}) (_, Build{started = b}) = compare a b

loadStringFromURL :: String -> IO String
loadStringFromURL url = do
  ret <- openURIWithOpts [CurlUseNetRc NetRcRequired] url
  case ret of
    Right b -> return $ BS.unpack b
    Left  _ -> return ""

loadBuilds =
  loadStringFromURL "https://git.smprc.ru/api/storage/squeeze/amd64/builds"

getBuildData = do
    bs <- loadBuilds
    return $ parseJSON bs

getBuildLog buildName = do
  log <- loadStringFromURL $ "https://git.smprc.ru/api/storage/squeeze/amd64/log/" ++ buildName
  case decode log of
    Ok (JSObject a) -> 
      case get_field a "log" of
        Nothing -> return ""
        Just (JSString s) -> return $ fromJSString s
    _ -> return ""

getBuildDiff buildName = do
  return ""