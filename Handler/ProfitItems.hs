{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.ProfitItems where

import Import
import Database.Persist.Sql (rawSql,RawSql(..))
import Text.Printf


data Profit = Profit
              { typeName :: Text
              , typeId :: Int64
              , quantity :: Int64
              , profitcents :: Int64
              , fee :: Int64
              , tax :: Int64
              , avg :: Int64
              , sellcents :: Int64
              } deriving (Show, Eq, Generic)

-- profitcents may be NULL (therfore also avg). Test for that. Rest ist never NULL, due to update-routine
instance RawSql Profit where
  rawSqlCols _ _ = (8,[])
  rawSqlColCountReason _ = "typeName, typeId, quantity, profit, fee, tax, avg, sell"
  rawSqlProcessRow [PersistText t, PersistInt64 i, PersistRational q,
               PersistRational pc, PersistRational bf, PersistRational tt,
               PersistRational a, PersistRational s] = Right (Profit t i (c q) (c pc) (c bf) (c tt) (c a) (c s))
                 where c = floor
  rawSqlProcessRow [PersistText t, PersistInt64 i, PersistRational q,
               PersistNull, PersistRational bf, PersistRational tt,
               PersistNull, PersistRational s] = Right (Profit t i (c q) 0 (c bf) (c tt) 0 (c s))
                 where c = floor
  rawSqlProcessRow a = Left ("Wrong kinds of Arguments:" <> (pack $ show a))

profitIntervals :: [Int64]
profitIntervals = [1,2,7,14,31]

getProfitItemsR :: Handler Html
getProfitItemsR = getProfitItemsDetailsR 7


getProfitItemsDetailsR :: Int64 -> Handler Html
getProfitItemsDetailsR days = loginOrDo (\(uid,user) -> do
             let sql = "select type_name, type_id, sum(quantity) as quantity, sum(profit) as profitcents,\
                               sum(fee) as brokerfee, sum(tax) as transactiontax,\
                               sum(seconds_to_sell)/(sum(CASE WHEN trans_is_sell THEN 1 ELSE 0 END)+0.01) as avg,\
                               sum(CASE WHEN trans_is_sell THEN quantity*price_cents ELSE 0 END) as sell\
                        from transaction\
                        where\
                              \"user\"=? and\
                              extract(day from now()-date(date_time) at time zone 'utc') < ?\
                              and not problematic\
                        group by type_id, type_name\
                        order by (coalesce(sum(profit),0)-sum(fee)-sum(tax)) desc"
             (items :: [Profit]) <- runDB $ rawSql sql [toPersistValue uid, toPersistValue days]
             loginLayout user $ [whamlet|
             <div .panel .panel-default>
               <div .panel-heading>Statistices for the last #{days} days:
               <div .btn-group .btn-group-justified role="group">
                 $forall days' <- profitIntervals
                   $if days == days'
                     <a href="@{ProfitItemsDetailsR days'}" .btn .active role="button">#{days'} days
                   $else
                     <a href="@{ProfitItemsDetailsR days'}" .btn role="button">#{days'} days
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Item
                   <th .text-center>ISK Profit
                   <th .text-center>ISK Broker Fee
                   <th .text-center>ISK Transaction Tax
                   <th .text-center>Real Profit
                   <th .text-center>Avg Time
                   <th .text-center># Items
                   <th .text-center>Real Profit/Day
                   <th .text-center>%
                   <th .text-center>%/Day
                 $forall (Profit tn tid quant pc f t a sc) <- items
                   <tr>
                     <td><a href="@{ItemR tid}">#{tn}</a>
                     <td .numeric>#{prettyISK pc}
                     <td .numeric>#{prettyISK f}
                     <td .numeric>#{prettyISK t}
                     <td .numeric>#{transRealProfit' pc f t}
                     <td .numeric>#{showSecsToSell a}
                     <td .numeric>#{quant}
                     <td .numeric>#{prettyISK $ profitPerDay pc f t a}
                     <td .numeric>#{profitPercent' pc f t sc}
                     <td .numeric>#{profitPercentDay pc f t sc a}
                  |]
                  )


transRealProfit' :: Int64 -> Int64 -> Int64 -> String
transRealProfit' p bf tt = prettyISK (p-bf-tt)

profitPerDay :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
profitPerDay _ _ _ 0 = 0
profitPerDay p f t a = ((p-f-t) * 86400) `div` a

profitPercent' :: Int64 -> Int64 -> Int64 -> Int64 -> String
profitPercent' _ _ _ 0 = printf "%.2f" $ (0 :: Double)
profitPercent' p f t s = printf "%.2f" $ (100*(fromIntegral (p-f-t)) / (fromIntegral s) :: Double)

profitPercentDay :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> String
profitPercentDay _ _ _ 0 _ = printf "%.2f" $ (0 :: Double)
profitPercentDay _ _ _ _ 0 = printf "%.2f" $ (0 :: Double)
profitPercentDay p f t s a = printf "%.2f" $ (100*(fromIntegral (profitPerDay p f t a)) / (fromIntegral s) :: Double)
