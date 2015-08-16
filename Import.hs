module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Yesod.Form.Bootstrap3 as Import
import Text.Printf
import Data.List (unfoldr)

loginOrDo :: ((Key User, User) -> Handler Html) -> Handler Html
loginOrDo cont = do
                 maid <- maybeAuthId
                 muid <- case maid of
                         Just uid -> fmap ((,) uid) <$> runDB (get uid)
                         Nothing -> return Nothing
                 case muid of
                      Nothing -> redirect (AuthR LoginR)
                      Just (uid,u) -> cont (uid,u)

prettyISK :: Int64 -> String
prettyISK isk = signIsk++pretty++","++ printf "%02u" cents
  where
    signIsk = if isk >= 0 then "" else "-"
    (isk',cents) = divMod (abs isk) 100
    thousands = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 1000, b `div` 1000)) isk'
    pretty = case reverse thousands of
               (ht:t) -> intercalate "." $ show ht : (printf "%03u" <$> t)
               [] -> "0"

showTime :: Int64 -> String
showTime t = printf "%2u" hours ++ ":" ++ printf "%02u" minutes ++ ":" ++ printf "%02u" seconds
  where
    (hours, minutes') = divMod t 3600
    (minutes, seconds) = divMod minutes' 60

showDateTime :: UTCTime -> String
showDateTime t = (show . utctDay $ t) ++ " " ++
                 (showTime . round . utctDayTime $ t)
