module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Yesod.Form.Bootstrap3 as Import
import Text.Printf
import Data.List (unfoldr)
import Text.Hamlet

{- CONVINIENCE FUNCTIONS -}

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
showTime t = printf "%02u" hours ++ ":" ++ printf "%02u" minutes ++ ":" ++ printf "%02u" seconds
  where
    (hours, minutes') = divMod t 3600
    (minutes, seconds) = divMod minutes' 60

showDateTime :: UTCTime -> String
showDateTime t = (show . utctDay $ t) ++ " " ++
                 (showTime . round . utctDayTime $ t)

loginLayout :: ToWidget App a =>
               User
               -> a
               -> HandlerT
               App IO Html
loginLayout user widget = do
    master <- getYesod
    mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_neat_css
        addScript $ StaticR js_jquery_js
        addScript $ StaticR js_bootstrap_js
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/login-layout-wrapper.hamlet")


showSecsToSell :: Int64 -> String
showSecsToSell t
  | t > 4*7*86400 = pp (fromIntegral t / (7*86400) :: Double) ++ "w"
  | t > 86400     = pp (fromIntegral t / 86400 :: Double) ++ "d"
  | t > 3600      = pp (fromIntegral t / 3600 :: Double) ++ "h"
  | t > 60        = pp (fromIntegral t / 60 :: Double) ++ "m"
  | t == 0        = "-"
  | otherwise     = pp (fromIntegral t :: Double) ++ "s"
  where
    pp = printf "%.2f"
