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
             trans <- runDB $ selectList
                                [TransactionDateTime >. (addUTCTime ((fromIntegral $ -(hrs*3600)) :: NominalDiffTime) now)
                                ,TransactionUser ==. uid
                                ]
                                [Desc TransactionDateTime]
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
             loginLayout user $ $(widgetFile "wallet")
             )

transRealProfit' :: Int64 -> Int64 -> Int64 -> String
transRealProfit' p bf tt = prettyISK (p-bf-tt)

profitPercent' :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe String
profitPercent' p bf tt s = if s == 0 then Nothing
                                     else Just . printf "%.2f" $ 100*(fromIntegral (p - bf - tt) / fromIntegral s :: Double)

addProfit :: ProfitSum -> Profit -> ProfitSum
addProfit (ProfitSum b' s' p' bf' tt') (Profit _ b s p bf tt) = ProfitSum (b+b') (s+s') (p+p') (bf+bf') (tt+tt')
