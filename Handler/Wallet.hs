module Handler.Wallet where

import Import

import Eve.Api.Char.MarketOrders
import Eve.Api.Types as T

getWalletR :: Handler Html
getWalletR = loginOrDo $ (\(uid,user) -> do
             man <- getHttpManager <$> ask
             apiKey <- runDB $ getBy $ UniqueApiUser uid
             acc <- case apiKey of
                      Just (Entity _ (Api _ k v)) -> do
                          a <- liftIO $ getMarketOrders man (mkComplete k v (userCharId user))
                          return (Just a)
                      Nothing -> return Nothing
             defaultLayout $ [whamlet|
             <h1>Transactions in the last xx hours

             <h1>Statistices for the last xx days
             #{show acc}
             |]
             )

