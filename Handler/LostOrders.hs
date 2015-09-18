{-# LANGUAGE ScopedTypeVariables #-}

module Handler.LostOrders where

import Import
import Database.Persist.Sql (rawSql,RawSql(..))

data Lost = Lost
          { maxtime :: UTCTime
          , realprofit :: Int64
          , typeId :: Int64
          , priceCents :: Int64
          , typeName :: Text
          , stationId :: Int64
          , stationName :: Text
          }

instance RawSql Lost where
  rawSqlCols _ _ = (7,[])
  rawSqlColCountReason _ = "maxtime, realprofit, typeid, pricecents, typename, stationid, stationname"
  rawSqlProcessRow [PersistUTCTime t, PersistRational p, PersistInt64 tid, PersistRational pc,
                   PersistText tn, PersistInt64 sid, PersistText sn] = Right $ Lost t (c p) tid (c pc) tn sid sn
                     where c = floor
  rawSqlProcessRow [PersistUTCTime t, PersistNull, PersistInt64 tid, PersistRational pc,
                   PersistText tn, PersistInt64 sid, PersistText sn] = Right $ Lost t 0 tid (c pc) tn sid sn
                     where c = floor
  rawSqlProcessRow a = Left ("Wrong kind of Arguments:" <> (pack $ show a))

lostOrderIntervals :: [Int]
lostOrderIntervals = [1,2,7,14,31]

getLostOrdersR :: Handler Html
getLostOrdersR = getLostOrdersDaysR 1

getLostOrdersDaysR :: Int -> Handler Html
getLostOrdersDaysR days = loginOrDo (\(uid,user) -> do
         let rawStmt = "SELECT \
                         max(date_time) as maxtime, COALESCE(sum(profit-tax-fee),0) as realprofit, type_id, \
                         avg(price_cents), type_name, station_id, station_name \
                       FROM \
                         transaction \
                       where \
                         \"user\"=? and date_time > CURRENT_TIMESTAMP - INTERVAL '? day' \
                         and type_id not in (SELECT distinct type_id FROM \"order\" where \"user\"=? and order_state=0) \
                         and type_id not in (select distinct type_id FROM transaction where \"user\"=? and date_time > CURRENT_TIMESTAMP - INTERVAL '? day' and in_stock > 0) \
                       group by \
                         type_id, type_name, station_id, station_name \
                       order by realprofit desc"
         lorders :: [Lost] <- runDB $ rawSql rawStmt [toPersistValue uid,toPersistValue days,toPersistValue uid,toPersistValue uid,toPersistValue days]
         loginLayout user $ [whamlet|
             <div .panel .panel-default>
               <div .panel-heading>Lost Orders in the last #{days} days:
               <div .btn-group .btn-group-justified role="group">
                 $forall days' <- lostOrderIntervals
                   $if days == days'
                     <a href="@{LostOrdersDaysR days'}" .btn .active role="button">#{days'} days
                   $else
                     <a href="@{LostOrdersDaysR days'}" .btn role="button">#{days'} days
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Item
                   <th .text-center>ISK Profit
                   <th .text-center>Avg Price
                   <th .text-center>Last Traded
                   <th .text-center>On Station
                 $forall (Lost t rp tid pc tn sid sn) <- lorders
                   <tr>
                     <td><a href="@{ItemR tid}">#{tn}</a>
                     <td .numeric>#{prettyISK rp}
                     <td .numeric>#{prettyISK pc}
                     <td>#{showDateTime t}
                     <td>#{sn}
         |]
         )
