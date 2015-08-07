module Handler.Wallet where

import Import

import Data.List (unfoldr)
import Data.Time.Clock
import Text.Printf

getWalletR :: Handler Html
getWalletR = getWalletDetailsR 6 7

getWalletDetailsR :: Int64 -> Int64 -> Handler Html
getWalletDetailsR hrs days = loginOrDo (\(uid,user) -> do
             now <- liftIO getCurrentTime
             trans <- runDB $ selectList [TransactionDateTime >. (addUTCTime ((fromIntegral $ -(hrs*3600)) :: NominalDiffTime) now)] [Desc TransactionDateTime]
             defaultLayout $ [whamlet|
             <a href=@{WalletDetailsR 168 days}>show last 7 days
             <h1>Transactions in the last #{hrs} hours
             <table .table>
               <tr>
                 <th>Time
                 <th>P/C
                 <th>B/S
                 <th>Item
                 <th>Quantity
                 <th>ISK/Item
                 <th>ISK total
                 <th>ISK profit
                 <th>%
                 <th>Time
                 <th>Client
                 <th>Station
                 <th>?
                 <th>
               $forall Entity _ t <- trans
                 <tr>
                   <td>#{show $ utctDay $ transactionDateTime $ t} #{show $ utctDayTime $ transactionDateTime $ t}
                   $if transactionTransForCorp t
                     <td .corpTransaction>C
                   $else
                     <td .personalTransaction>P
                   $if transactionTransIsSell t
                     <td .sellTransaction>S
                   $else
                     <td .buyTransaction>B
                   <td>#{transactionTypeName t}
                   <td>#{transactionQuantity t}
                   <td>#{prettyISK $ transactionPriceCents t}
                   <td>#{prettyISK $ transactionQuantity t * transactionPriceCents t}
                   $maybe profit <- transRealProfit t
                     <td>
                       #{prettyISK $ profit}
                     <td>
                       #{profitPercent profit t}
                   $nothing
                     <td>
                       -
                     <td>
                   <td>
                     $maybe secs <- transactionSecondsToSell t
                       #{secs}
                     $nothing
                       &nbsp;
                   <td>#{transactionClientName t}
                   <td>#{transactionStationName t}
                   <td>
                   <td>

             <h1>Statistices for the last #{days} days
             |]
             )

transRealProfit :: Transaction -> Maybe Int64
transRealProfit t = (\a b c -> a - b - c) <$> transactionProfit t <*> transactionFee t <*> transactionTax t

profitPercent :: Int64 -> Transaction -> String
profitPercent p t = printf "%.2f" $ (100*(fromIntegral p) / (fromIntegral (transactionQuantity t * transactionPriceCents t)) :: Double)

prettyISK :: Int64 -> String
prettyISK isk = pretty++","++ printf "%02u" cents
  where
    (isk',cents) = divMod isk 100
    thousands = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 1000, b `div` 1000)) isk'
    (ht:t) = reverse thousands
    pretty = intercalate "." $ [show ht] ++ (printf "%03u" <$> t)


