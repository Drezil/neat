{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Stock where

import Import
--import Database.Esqueleto as E
import Database.Persist.Sql (rawSql,RawSql(..))


data Stock = Stock
           { typeId :: Int64
           , stationId :: Int64
           , stationName :: Text
           , typeName :: Text
           , inStock :: Rational
           , worth :: Rational
           , datetime :: UTCTime
           , tax :: Double
           } deriving (Show, Eq)

data DisCols = DisCols
             { dTypeId :: Int64
             , dStationId :: Int64
             , dStationName :: Text
             , dTypeName :: Text
             , dInStock :: Int64
             , dWorth :: Int64
             , dAvgWorth :: Int64
             , dDateTime :: UTCTime
             , dResell :: Int64
             } deriving (Show, Eq)

instance RawSql Stock where
  rawSqlCols _ _ = (8,[])
  rawSqlColCountReason _ = "typeId, stationId, stationName, typeName, inStock, worth, date, tax"
  rawSqlProcessRow [PersistInt64 t, PersistInt64 s, PersistText sn, PersistText tn,
               PersistRational is, PersistRational w, PersistUTCTime time, PersistDouble tax] =
                  Right (Stock t s sn tn is w time tax)
  rawSqlProcessRow a = Left ("Wrong kinds of Arguments:" <> (pack $ show a))

getStockR :: Handler Html
getStockR = loginOrDo (\(uid,user) -> do
              --items <- runDB $ selectList [TransactionUser ==. uid, TransactionInStock >. 0] [Asc TransactionTypeName]
              --persist cannot do groupBy
              {-(items :: [(E.Value Text,
                          E.Value Text,
                          E.Value (Maybe Rational),
                          E.Value (Maybe Rational))]) <-
                         runDB $ select $ from $ \trans -> do
                                 where_ (trans ^. TransactionInStock E.>. (val 0))
                                 E.groupBy $ trans ^. TransactionTypeId
                                 E.groupBy $ trans ^. TransactionTypeName
                                 E.groupBy $ trans ^. TransactionStationName
                                 orderBy [asc (trans ^. TransactionTypeName)]
                                 return (trans ^. TransactionTypeName
                                        ,trans ^. TransactionStationName
                                        ,sum_ $ trans ^. TransactionInStock
                                        ,avg_ $ (trans ^. TransactionPriceCents) *. (trans ^. TransactionInStock))-}
              --esqueleto does not work because we reference tables outside of the model, so we come back to raw SQL:
              let sql = "select t.type_id, t.station_id, t.station_name, t.type_name, \
                                sum(t.in_stock) as in_stock, \
                                sum(t.in_stock*t.price_cents) as worth, \
                                to_timestamp(avg(extract(epoch from t.date_time at time zone 'utc'))) at time zone 'utc' as date_time, \
                                (0.01-(0.001*ch.acc))+2*(0.0100-0.0005*ch.br)/exp(0.1000*COALESCE(\
                                         (select faction_standing from faction_standings where faction_id=c.\"factionID\" and \"user\"=t.\"user\")\
                                ,0)+0.0400*COALESCE(\
                                         (select corp_standing from corp_standings where corp_id=c.\"corporationID\" and \"user\"=t.\"user\")\
                                ,0))+1 as tax \
                                \
                           from transaction t \
                                join \"staStations\" s on (t.station_id = s.\"stationID\") \
                                join \"crpNPCCorporations\" c on (s.\"corporationID\" = c.\"corporationID\") \
                                join \"user\" ch on (t.\"user\"=ch.id) \
                                \
                           where t.\"user\" = ? \
                                and t.in_stock > 0 and not trans_is_sell \
                                \
                           group by t.type_id, t.station_id, t.type_name, t.station_name,\
                                    ch.acc, ch.br, c.\"factionID\", t.\"user\", c.\"corporationID\" \
                           order by t.type_name asc"
              (items :: [Stock]) <- runDB $ rawSql sql [toPersistValue uid]
              let items' = convertStock <$> items
              let total = foldl' sumTotal 0 items'
              loginLayout user $ [whamlet|
             <div .panel .panel-default>
               <div .panel-heading>Current Stock:
               <table .table .table-condensed .small .table-bordered>
                 <tr>
                   <th .text-center>Bought
                   <th .text-center>Item name
                   <th .text-center>Quantity
                   <th .text-center>Buy price
                   <th .text-center>Required sell
                   <th .text-center>Total
                   <th .text-center>Station name
                 $forall DisCols tid sid sn tn is wrth avg' dt taxed <- items'
                   <tr>
                     <td>#{showDateTime dt}
                     <td>#{tn}
                     <td .numeric>#{is}
                     <td .numeric>#{prettyISK avg'}
                     <td .numeric>#{prettyISK taxed}
                     <td .numeric>#{prettyISK wrth}
                     <td>#{sn}
                 <tr .total>
                   <th .text-center>Total
                   <td>
                   <td .numeric>
                   <td .numeric>
                   <td .numeric>
                   <td .numeric>#{prettyISK total}
                   <td>
             |]
            )

convertStock :: Stock -> DisCols
convertStock (Stock tid sid sn tn is wrth dt tax) = DisCols tid sid sn tn (floor is) (floor wrth) avgItem dt (floor $ (fromIntegral avgItem) * tax)
  where
    avgItem = floor $ wrth / is


sumTotal :: Int64 -> DisCols -> Int64
sumTotal t (DisCols _ _ _ _ _ t' _ _ _) = t + t'
