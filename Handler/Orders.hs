{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Orders where

import Import
import Database.Persist.Sql
import qualified Eve.Api.Char.MarketOrders as MO

getOrdersR :: Handler Html
getOrdersR = loginOrDo (\(uid,user) -> do
               -- using raw because model does not know about CCP-Data-Dump
               let sql = "select ??, t.\"typeName\", s.\"stationName\", s.\"regionID\" from \"order\" join \"invTypes\" t on (\"order\".type_id = t.\"typeID\") join \"staStations\" s on (\"order\".station_id = s.\"stationID\") where \"user\"=? order by \"order\".is_sell asc, t.\"typeName\" asc"
               (orders :: [(Entity Order, Single Text, Single Text, Single Text)]) <- runDB $ rawSql sql [toPersistValue uid]
               let f = \(a,_,_,_) -> a
               let sellorders = filter (\(Entity _ o, _, _, _) -> orderIsSell o && orderOrderState o == (fromIntegral . fromEnum) MO.Open) orders
               let sellsum = foldl' sumOrders 0 (f <$> sellorders)
               let sellescrow = foldl' sumEscrow 0 (f <$> sellorders)
               let buyorders = filter (\(Entity _ o, _, _, _) -> not (orderIsSell o) && orderOrderState o == (fromIntegral . fromEnum) MO.Open) orders
               let buysum = foldl' sumOrders 0 (f <$> buyorders)
               let buyescrow = foldl' sumEscrow 0 (f <$> buyorders)
               loginLayout user [whamlet|
               <div class="btn-group btn-group-justified">
                 <div class="btn-group">
                   <button type="button" class="btn btn-primary" onclick="checkOrders();">Check Over/Undercut
                 <div class="btn-group">
                   <button type="button" class="btn btn-primary">Feature x
                 <div class="btn-group">
                   <button type="button" class="btn btn-primary">Feature y
               <div .panel .panel-default>
                 <div .panel-heading>Current Sell Orders:
                 <table .table .table-condensed .small .sellOrders>
                   <tr>
                     <th .text-center>Last changed
                     <th .text-center>Item
                     <th .text-center>Price
                     <th .text-center>Quantity (min)
                     <th .text-center>Value
                     <th .text-center>Range
                     <th .text-center>Duration
                     <th .text-center>Escrow
                     <th .text-center>Station
                   $forall (Entity _ o, Single name, Single stationname, Single regionid) <- sellorders
                     <tr .order data="#{orderTypeId o};#{regionid};#{orderPriceCents o}">
                       <td>#{showDateTime $ orderIssued $ o}
                       <td>#{name}
                       <td .numeric .price>#{prettyISK $ orderPriceCents o}
                       <td .numeric>#{orderVolRemaining o}/#{orderVolEntered o} (#{orderMinVolume o})
                       <td .numeric>#{prettyISK $ orderVolRemaining o * orderPriceCents o}
                       <td .numeric>#{prettyRange $ orderRange o}
                       <td .numeric>#{orderDuration o}
                       <td .numeric>#{prettyISK $ orderEscrowCents o}
                       <td>#{stationname}
                   <tr .total>
                     <td>Total
                     <td>
                     <td>
                     <td>
                     <td .numeric>#{prettyISK $ sellsum}
                     <td>
                     <td>
                     <td .numeric>#{prettyISK $ sellescrow}
                     <td>
               <div .panel .panel-default>
                 <div .panel-heading>Current Buy Orders:
                 <table .table .table-condensed .small .buyOrders>
                   <tr>
                     <th .text-center>Last changed
                     <th .text-center>Item
                     <th .text-center>Price
                     <th .text-center>Quantity (min)
                     <th .text-center>Value
                     <th .text-center>Range
                     <th .text-center>Duration
                     <th .text-center>Escrow
                     <th .text-center>Station
                   $forall (Entity _ o, Single name, Single stationname, Single regionid) <- buyorders
                     <tr .order data="#{orderTypeId o};#{regionid};#{orderPriceCents o}">
                       <td>#{showDateTime $ orderIssued $ o}
                       <td>#{name}
                       <td .numeric .price>#{prettyISK $ orderPriceCents o}
                       <td .numeric>#{orderVolRemaining o}/#{orderVolEntered o} (#{orderMinVolume o})
                       <td .numeric>#{prettyISK $ orderPriceCents o * orderVolRemaining o}
                       <td .numeric>#{prettyRange $ orderRange o}
                       <td .numeric>#{orderDuration o}
                       <td .numeric>#{prettyISK $ orderEscrowCents o}
                       <td>#{stationname}
                   <tr .total>
                     <td>Total
                     <td>
                     <td>
                     <td>
                     <td .numeric>#{prettyISK $ buysum}
                     <td>
                     <td>
                     <td .numeric>#{prettyISK $ buyescrow}
                     <td>

               |]
             )

sumOrders :: Int64 -> Entity Order -> Int64
sumOrders s (Entity _ o) = s + orderPriceCents o * orderVolRemaining o

sumEscrow :: Int64 -> Entity Order -> Int64
sumEscrow s (Entity _ o) = s + orderEscrowCents o

prettyRange :: Int32 -> String
prettyRange = show . (toEnum :: Int -> MO.Range) . fromIntegral
