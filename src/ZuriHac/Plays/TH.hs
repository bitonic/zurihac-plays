module ZuriHac.Plays.TH
  ( Aeson.deriveJSON
  , jsonOptions
  , jsonProduct
  , jsonSum
  ) where

import qualified Data.Aeson.TH as Aeson

import ZuriHac.Plays.Prelude

stripPrefixOrError :: HasCallStack => Bool -> String -> String -> String
stripPrefixOrError lowerCase prefix str = case stripPrefix prefix str of
  Nothing -> error (str ++ " didn't match prefix " ++ prefix)
  Just (c : cs)
    | lowerCase -> toLower c : cs
    | otherwise -> c : cs
  _ -> error (str ++ " empty after stripping prefix " ++ prefix)

jsonOptions :: Maybe String -> Maybe String -> Aeson.Options
jsonOptions cstrPrefix fieldPrefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = maybe id (stripPrefixOrError True) fieldPrefix
  , Aeson.constructorTagModifier = maybe id (stripPrefixOrError False) cstrPrefix
  , Aeson.allNullaryToStringTag = True
  , Aeson.sumEncoding = Aeson.ObjectWithSingleField
  }

jsonProduct :: String -> Aeson.Options
jsonProduct fieldPrefix = jsonOptions Nothing (Just fieldPrefix)

jsonSum :: String -> Aeson.Options
jsonSum cstrPrefix = jsonOptions (Just cstrPrefix) Nothing
