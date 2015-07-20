module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Yesod.Form.Bootstrap3 as Import

loginOrDo :: (User -> Handler Html) -> Handler Html
loginOrDo cont = do
                 maid <- maybeAuthId
                 muid <- case maid of
                         Just uid -> runDB $ get uid
                         Nothing -> return Nothing
                 case muid of
                      Nothing -> redirect (AuthR LoginR)
                      Just u -> cont u
