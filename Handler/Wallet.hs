{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Wallet where

import Import

import Data.Time.Clock
import Text.Printf
import Database.Persist.Sql

data Profit = Profit
              { date :: Day
              , buy :: Int64
              , sell :: Int64
              , profit :: Int64
              , bf :: Int64
              , tt :: Int64
              } deriving (Show, Eq)

data ProfitSum = ProfitSum
                 { psbuy :: Int64
                 , pssell :: Int64
                 , psprofit :: Int64
                 , psbf :: Int64
                 , pstt :: Int64
                 } deriving (Show, Eq)

instance RawSql Profit where
  rawSqlCols _ _ = (6,[])
  rawSqlColCountReason _ = "date, buy, sell, profit, bf, tt"
  rawSqlProcessRow [PersistDay t, PersistInt64 b, PersistInt64 s,
               PersistInt64 p, PersistInt64 bf, PersistInt64 tt] = Right (Profit t b s p bf tt)
  rawSqlProcessRow a = Left ("Wrong kinds of Arguments:" <> (pack $ show a))

buttonIntervals :: [(Int64,String)]
buttonIntervals = [ (2,"2 hours")
                  , (6,"6 hours")
                  , (12,"12 hours")
                  , (24,"1 day")
                  , (48,"2 days")
                  , (7*24,"7 days")
                  , (31*24,"31 days")
                  ]

profitIntervals :: [Int64]
profitIntervals = [7,14,31]

getWalletR :: Handler Html
getWalletR = getWalletDetailsR 6 7

getWalletDetailsR :: Int64 -> Int64 -> Handler Html
getWalletDetailsR hrs days = loginOrDo (\(uid,user) -> do
             now <- liftIO getCurrentTime
             trans <- runDB $ selectList [TransactionDateTime >. (addUTCTime ((fromIntegral $ -(hrs*3600)) :: NominalDiffTime) now)] [Desc TransactionDateTime]
             let profitquery = "select \
                                  min(date(date_time at time zone 'utc')) as date,\
                                  sum(CASE WHEN NOT trans_is_sell THEN quantity*price_cents ELSE 0 END) :: bigint as buy,\
                                  sum(CASE WHEN trans_is_sell THEN quantity*price_cents ELSE 0 END) :: bigint as sell,\
                                  sum(COALESCE(profit,0)) :: bigint as profit,\
                                  sum(fee) :: bigint as brokerfee,\
                                  sum(tax) :: bigint as transactiontax \
                                from transaction \
                                where \
                                  \"user\"=? \
                                  and extract(day from (now() at time zone 'utc')-date(date_time at time zone 'utc')) < ? \
                                group by \
                                  extract(day from (now() at time zone 'utc')-date(date_time at time zone 'utc')) \
                                order by \
                                  extract(day from (now() at time zone 'utc')-date(date_time at time zone 'utc')) asc"
             (profits :: [Profit]) <- runDB $ rawSql profitquery [toPersistValue uid, toPersistValue days]
             let profitssum = foldl' addProfit (ProfitSum 0 0 0 0 0) profits
             loginLayout user $ [whamlet|
             <div .panel .panel-default>
               <div .panel-heading>Transactions in the last #{hrs} hours:
               <div .btn-group .btn-group-justified role="group">
                 $forall (hrs',cap) <- buttonIntervals
                   $if hrs == hrs'
                     <a href="@{WalletDetailsR hrs' days}" .btn .active role="button">#{cap}
                   $else
                     <a href="@{WalletDetailsR hrs' days}" .btn role="button">#{cap}
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Time
                   <th .text-center>P/C
                   <th .text-center>B/S
                   <th .text-center>Item
                   <th .text-center>##
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
                     <td>#{showDateTime $ transactionDateTime $ t}
                     $if transactionTransForCorp t
                       <td .corpTransaction .text-center>C
                     $else
                       <td .personalTransaction .text-center>P
                     $if transactionTransIsSell t
                       <td .sellTransaction .text-center>S
                     $else
                       <td .buyTransaction .text-center>B
                     <td>#{transactionTypeName t}
                     <td .numeric>#{transactionQuantity t}
                     <td .numeric>#{prettyISK $ transactionPriceCents t}
                     <td .numeric>#{prettyISK $ transactionQuantity t * transactionPriceCents t}
                     $maybe profit <- transRealProfit t
                       $if (&&) (transactionTransIsSell t) (profit > 0)
                         <td .numeric .profit>
                           #{prettyISK $ profit}
                       $elseif (&&) (transactionTransIsSell t) (profit < 0)
                         <td .numeric .loss>
                           #{prettyISK $ profit}
                       $elseif not (transactionTransIsSell t)
                         <td .numeric .buyfee>
                           #{prettyISK $ profit}
                       $else
                         <td .numeric>
                           #{prettyISK $ profit}
                       <td .numeric>
                         #{profitPercent profit t}%
                     $nothing
                       <td>
                         -
                       <td>
                     <td .duration>
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
               <div .btn-group .btn-group-justified role="group">
                 $forall days' <- profitIntervals
                   $if days == days'
                     <a href="@{WalletDetailsR hrs days'}" .btn .active role="button">#{days'} days
                   $else
                     <a href="@{WalletDetailsR hrs days'}" .btn role="button">#{days'} days
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Date
                   <th .text-center>ISK Buy
                   <th .text-center>ISK Sell
                   <th .text-center>ISK Profit
                   <th .text-center>ISK Broker Fee
                   <th .text-center>ISK Transaction Tax
                   <th .text-center>Real Profit
                   <th .text-center>%
                 $forall (Profit t b s p bf tt) <- profits
                   <tr>
                     <td>#{show t}
                     <td .numeric>#{prettyISK b}
                     <td .numeric>#{prettyISK s}
                     <td .numeric>#{prettyISK p}
                     <td .numeric>#{prettyISK bf}
                     <td .numeric>#{prettyISK tt}
                     <td .numeric>#{transRealProfit' p bf tt}
                     <td .numeric>
                       $maybe pp <- profitPercent' p bf tt s
                         #{pp}
                       $nothing
                         &nbsp;
                 $with (ProfitSum b s p bf tt) <- profitssum
                   <tr .total>
                     <th .text-center>Total
                     <td .numeric>#{prettyISK b}
                     <td .numeric>#{prettyISK s}
                     <td .numeric>#{prettyISK p}
                     <td .numeric>#{prettyISK bf}
                     <td .numeric>#{prettyISK tt}
                     <td .numeric>#{transRealProfit' p bf tt}
                     <td .numeric>
                       $maybe pp <- profitPercent' p bf tt s
                         #{pp}
                       $nothing
                         &nbsp;
             |]
             )

transRealProfit :: Transaction -> Maybe Int64
transRealProfit t = if transactionTransIsSell t then
                      (\a b c -> a - b - c) <$> transactionProfit t <*> transactionFee t <*> transactionTax t
                    else
                      negate <$> ((+) <$> transactionFee t <*> transactionTax t)

transRealProfit' :: Int64 -> Int64 -> Int64 -> String
transRealProfit' p bf tt = prettyISK (p-bf-tt)

profitPercent' :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe String
profitPercent' p bf tt s = if s == 0 then Nothing
                                     else Just . printf "%.2f" $ 100*(fromIntegral (p - bf - tt) / fromIntegral s :: Double)

profitPercent :: Int64 -> Transaction -> String
profitPercent p t = printf "%.2f" $ (100*(fromIntegral p) / (fromIntegral (transactionQuantity t * transactionPriceCents t)) :: Double)

addProfit :: ProfitSum -> Profit -> ProfitSum
addProfit (ProfitSum b' s' p' bf' tt') (Profit _ b s p bf tt) = ProfitSum (b+b') (s+s') (p+p') (bf+bf') (tt+tt')
