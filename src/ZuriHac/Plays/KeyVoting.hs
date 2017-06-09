module ZuriHac.Plays.KeyVoting where

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS

import ZuriHac.Plays.Protocol
import ZuriHac.Plays.Prelude

type KeyGroup = Int

data KeysConfig = KeysConfig
  { kcPercentageRequired :: Double
  , kcKeyGroups :: HMS.HashMap KeyCode KeyGroup
  }

type User = ByteString

data UserState = UserState
  { usPressedKeys :: HS.HashSet KeyCode
  , usReleasedKeys :: HS.HashSet KeyCode
  }

type UserStates = HMS.HashMap User UserState

processEvent :: UserStates -> User -> Event -> UserStates
processEvent uss user evt = case HMS.lookup user uss of
  Nothing -> error ("processEvent: cound not find user " <> show user)
  Just us@UserState{usPressedKeys, usReleasedKeys} -> let
    us' = case evt of
      EventKeyPress kc -> us
        { usPressedKeys = HS.insert kc usPressedKeys
        , usReleasedKeys = HS.delete kc usReleasedKeys
        }
      EventKeyRelease kc -> us
        { usReleasedKeys = HS.insert kc usReleasedKeys }
    in HMS.insert user us' uss

type KeyCount = Int

finishRound :: KeysConfig -> UserStates -> (UserStates, HS.HashSet KeyCode)
finishRound kconf uss = let
  kcs :: HMS.HashMap KeyGroup (HMS.HashMap KeyCode KeyCount)
  kcs = foldl' (HMS.unionWith (HMS.unionWith (+))) mempty $ do
    UserState{usPressedKeys} <- HMS.elems uss
    kc <- HS.toList usPressedKeys
    Just kg <- return (HMS.lookup kc (kcKeyGroups kconf))
    return (HMS.singleton kg (HMS.singleton kc 1))
  numUsers = fromIntegral (HMS.size uss)
  goodKcs = HS.fromList $ do
    kcs <- HMS.elems kcs
    let (kc, count) : _ = sortBy (comparing (Down . snd)) (HMS.toList kcs)
    guard (fromIntegral count / numUsers > kcPercentageRequired kconf)
    return kc
  uss' = HMS.fromList $ do
    (user, us) <- HMS.toList uss
    return
      ( user
      , us
          { usPressedKeys = usPressedKeys us `HS.difference` usReleasedKeys us
          , usReleasedKeys = mempty
          }
      )
  in (uss', goodKcs)
