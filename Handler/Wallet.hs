module Handler.Wallet where

import Import

import Data.List (unfoldr)
import Data.Time.Clock
import Text.Printf

buttonIntervals :: [(Int64,String)]
buttonIntervals = [ (2,"2 hours")
                  , (6,"6 hours")
                  , (12,"12 hours")
                  , (24,"1 day")
                  , (48,"2 days")
                  , (7*24,"7 days")
                  , (31*24,"31 days")
                  ]

getWalletR :: Handler Html
getWalletR = getWalletDetailsR 6 7

getWalletDetailsR :: Int64 -> Int64 -> Handler Html
getWalletDetailsR hrs days = loginOrDo (\(uid,user) -> do
             now <- liftIO getCurrentTime
             trans <- runDB $ selectList [TransactionDateTime >. (addUTCTime ((fromIntegral $ -(hrs*3600)) :: NominalDiffTime) now)] [Desc TransactionDateTime]
             defaultLayout $ [whamlet|
             <div .panel .panel-default>
               <div .panel-heading>Transactions in the last #{hrs} hours:
               <div .btn-group .btn-group-justified role="group">
                 $forall (hrs',cap) <- buttonIntervals
                   $if hrs == hrs'
                     <a href="@{WalletDetailsR hrs' days}" .btn .active role="button">#{cap}
                   $else
                     <a href="@{WalletDetailsR hrs' days}" .btn role="button">#{cap}
               <table .table .table-striped .table-condensed .small>
                 <tr>
                   <th .text-center>Time
                   <th .text-center>P/C
                   <th .text-center>B/S
                   <th .text-center>Item
                   <th .text-center>Quantity
                   <th .text-center>ISK/Item
                   <th .text-center>ISK total
                   <th .text-center>ISK profit
                   <th .text-center>%
                   <th .text-center>Time
                   <th .text-center>Client
                   <th .text-center>Station
                   <th .text-center>?
                   <th .text-center>
                 $forall Entity _ t <- trans
                   <tr>
                     <td>#{show $ utctDay $ transactionDateTime $ t} #{showTime $ round $ utctDayTime $ transactionDateTime $ t}
                     $if transactionTransForCorp t
                       <td .corpTransaction .text-center>C
                     $else
                       <td .personalTransaction .text-center>P
                     $if transactionTransIsSell t
                       <td .sellTransaction .text-center>S
                     $else
                       <td .buyTransaction .text-center>B
                     <td>#{transactionTypeName t}
                     <td .text-right>#{transactionQuantity t}
                     <td .text-right>#{prettyISK $ transactionPriceCents t}
                     <td .text-right>#{prettyISK $ transactionQuantity t * transactionPriceCents t}
                     $maybe profit <- transRealProfit t
                       $if (&&) (transactionTransIsSell t) (profit > 0)
                         <td .text-right .profit>
                           #{prettyISK $ profit}
                       $elseif (&&) (transactionTransIsSell t) (profit < 0)
                         <td .text-right .loss>
                           #{prettyISK $ profit}
                       $elseif (transactionTransIsSell t)
                         <td .text-right .buyfee>
                           #{prettyISK $ profit}
                       $else
                         <td .text-right>
                           #{prettyISK $ profit}
                       <td .text-right>
                         #{profitPercent profit t}%
                     $nothing
                       <td>
                         -
                       <td>
                     <td>
                       $maybe secs <- transactionSecondsToSell t
                         #{showSecsToSell secs}
                       $nothing
                         &nbsp;
                     <td>#{transactionClientName t}
                     <td>#{transactionStationName t}
                     <td>
                     <td>

             <div .panel .panel-default>
               <div .panel-heading>Statistices for the last #{days} days:
               <table .table .table-striped .table-condensed .small>
                 <tr>
                   <th .text-center>TODO
             |]
             )

transRealProfit :: Transaction -> Maybe Int64
transRealProfit t = if transactionTransIsSell t then
                      (\a b c -> a - b - c) <$> transactionProfit t <*> transactionFee t <*> transactionTax t
                    else
                      negate <$> ((+) <$> transactionFee t <*> transactionTax t)

profitPercent :: Int64 -> Transaction -> String
profitPercent p t = printf "%.2f" $ (100*(fromIntegral p) / (fromIntegral (transactionQuantity t * transactionPriceCents t)) :: Double)

prettyISK :: Int64 -> String
prettyISK isk = signIsk++pretty++","++ printf "%02u" cents
  where
    signIsk = if isk > 0 then "" else "-"
    (isk',cents) = divMod (abs isk) 100
    thousands = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 1000, b `div` 1000)) isk'
    (ht:t) = reverse thousands
    pretty = intercalate "." $ [show ht] ++ (printf "%03u" <$> t)

showTime :: Int64 -> String
showTime t = printf "%2u" hours ++ ":" ++ printf "%02u" minutes ++ ":" ++ printf "%02u" seconds
  where
    (hours, minutes') = divMod t 3600
    (minutes, seconds) = divMod minutes' 60

showSecsToSell :: Int64 -> String
showSecsToSell t
  | t > 4*7*86400 = pp (fromIntegral t / (7*86400) :: Double) ++ "w"
  | t > 86400     = pp (fromIntegral t / 86400 :: Double) ++ "d"
  | t > 3600      = pp (fromIntegral t / 3600 :: Double) ++ "h"
  | t > 60        = pp (fromIntegral t / 60 :: Double) ++ "m"
  | otherwise     = pp (fromIntegral t :: Double) ++ "s"
  where
    pp x = printf "%.2f" x
