module Handler.Wallet where

import Import

import Data.Time.Clock

getWalletR :: Handler Html
getWalletR = getWalletDetailsR 6 7

getWalletDetailsR :: Int64 -> Int64 -> Handler Html
getWalletDetailsR hrs days = loginOrDo (\(uid,user) -> do
             now <- liftIO getCurrentTime
             trans <- runDB $ selectList [TransactionDateTime >. (addUTCTime ((fromIntegral $ -(hrs*3600)) :: NominalDiffTime) now)] [Desc TransactionDateTime]
             defaultLayout $ [whamlet|
             <a href=@{WalletDetailsR 168 days}>show last 7 days
             <h1>Transactions in the last #{hrs} hours
             <table>
               <tr>
                 <th>Time
                 <th>Price
                 <th>Name
                 <th>Profit
               $forall Entity _ t <- trans
                 <tr>
                   <td>#{show $ transactionDateTime t}
                   <td>#{transactionPriceCents t}
                   <td>#{transactionClientName t}
                   <td>
                     $maybe profit <- transRealProfit t
                       #{profit}
                     $nothing
                       -

             <h1>Statistices for the last #{days} days
             |]
             )

transRealProfit :: Transaction -> Maybe Int64
transRealProfit t = (\a b c -> a - b - c) <$> transactionProfit t <*> transactionFee t <*> transactionTax t
