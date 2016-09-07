{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-

This is basically very manual test. Check following link for details.

Google web oauth: https://developer.linkedin.com/docs/oauth2

-}

module Main where

import           Control.Applicative
import           Control.Monad        (mzero)
import           Data.Aeson
import qualified Data.ByteString      as BS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.HTTP.Conduit

import           Network.OAuth.OAuth2

import           Keys


main :: IO ()
main = do
    let state = "testLinkedInApi"
    print $ authorizationUrl linkedInKey `appendQueryParam` [("state", state)]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    mgr <- newManager tlsManagerSettings
    let (url, body) = accessTokenUrl linkedInKey (sToBS code)
    token <- doJSONPostRequest mgr linkedInKey url (body ++ [("state", state)])
    print (token :: OAuth2Result AccessToken)
    case token of
      Right at  -> userInfo mgr at >>= print
      Left _    -> putStrLn "no access token found yet"


-- | Test API: user
--
userInfo :: Manager -> AccessToken -> IO (OAuth2Result LinkedInUser)
userInfo mgr token = authGetJSON mgr token "https://api.linkedin.com/v1/people/~?format=json"

data LinkedInUser = LinkedInUser { lid        :: Text
                                 , lfirstName :: Text
                                 , llastName  :: Text
                                 } deriving (Show, Eq)

instance FromJSON LinkedInUser where
    parseJSON (Object o) = LinkedInUser
                           <$> o .: "id"
                           <*> o .: "firstName"
                           <*> o .: "lastName"
    parseJSON _ = mzero

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
