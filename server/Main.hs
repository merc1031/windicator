{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
import Servant
import Windicator
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Scientific (toBoundedInteger)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)

type Head = Verb HEAD 200

type WindicatorAPI = "windicator" :> ReqBody '[JSON] WindicatorRequest :> Post '[JSON] WindicatorResult
    :<|> "up" :> Head '[JSON] NoContent

type WindicatorResult = [[WeightedDart]]



data WindicatorRequest = WindicatorRequest
    { wrGoal :: Integer
    , wrThrowsRemaining :: Integer
    , wrHighest :: Integer
    , wrHistory :: [Dart]
    , wrLimit :: Maybe Integer
    }

instance FromJSON WindicatorRequest where
    parseJSON (Object v) =
        WindicatorRequest <$> v .: "goal"
                          <*> v .: "throws"
                          <*> v .: "highest"
                          <*> (fmap (map historyDartToDart) (v .: "history"))
                          <*> v .:? "limit"

data HistoryDart = HistoryDart
    { hdType :: DartNum'
    , hdNumber :: SimpleDartType
    }

instance FromJSON HistoryDart where
    parseJSON (Object v) =
        HistoryDart <$> v .: "type"
                    <*> v .: "number"

newtype DartNum' = DartNum' DartNum

instance FromJSON DartNum' where
    parseJSON (String v) = return $ DartNum' $ case v of
        "SINGLE_OUTER" -> Single
        "SINGLE_INNER" -> Single
        "DOUBLE" -> Double
        "TRIPLE" -> Triple


instance FromJSON SimpleDartType where
    parseJSON v@(Number s) = toDart (toBoundedInteger s)
        where toDart :: Maybe Int -> Parser SimpleDartType
              toDart (Just d) = return $ simpleDartFromInt $ fromIntegral d
              toDart Nothing = typeMismatch "DartNum" v

historyDartToDart :: HistoryDart -> Dart
historyDartToDart (HistoryDart {..}) = Dart (Simple hdNumber $ simplify hdType)
    where simplify (DartNum' s) = s

server :: Server WindicatorAPI
server = windicator :<|> up

windicatorAPI :: Proxy WindicatorAPI
windicatorAPI = Proxy

app :: Application
app = serve windicatorAPI server

printJSON :: ToJSON a => a -> IO ()
printJSON = BSLC.putStrLn . encodePretty

windicator :: WindicatorRequest -> Handler WindicatorResult
windicator (WindicatorRequest {..}) = return $ take' wrLimit $ calculate (State wrHighest) wrGoal wrThrowsRemaining wrHistory
    where take' (Just limit) = take $ fromIntegral limit
          take' Nothing = id

up :: Handler NoContent
up = return NoContent

main :: IO ()
main = run 8888 app

