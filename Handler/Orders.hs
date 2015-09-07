{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Orders where

import Import
import Database.Persist.Sql
import qualified Eve.Api.Char.MarketOrders as MO

getOrdersR :: Handler Html
getOrdersR = loginOrDo (\(uid,user) -> do
               -- using raw because model does not know about CCP-Data-Dump
               let sql = "select ??, t.\"typeName\" from \"order\" join \"invTypes\" t on (\"order\".type_id = t.\"typeID\") where \"user\"=? order by \"order\".is_sell asc, t.\"typeName\" asc"
               (orders :: [(Entity Order, Single Text)]) <- runDB $ rawSql sql [toPersistValue uid]
               let sellorders = filter (\(Entity _ o, _) -> orderIsSell o && orderOrderState o == (fromIntegral . fromEnum) MO.Open) orders
               let buyorders = filter (\(Entity _ o, _) -> not (orderIsSell o) && orderOrderState o == (fromIntegral . fromEnum) MO.Open) orders
               loginLayout user [whamlet|
               <div .panel .panel-default>
               <div .panel-heading>Current Sell Orders:
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Last changed
                   <th .text-center>Item
                   <th .text-center>Price
                   <th .text-center>Quantity (min)
                   <th .text-center>Range
                   <th .text-center>Duration
                   <th .text-center>Escrow
                   <th .text-center>Station
                 $forall (Entity _ o, Single name) <- sellorders
                   <tr>
                     <td>#{showDateTime $ orderIssued $ o}
                     <td>#{name}
                     <td .numeric>#{prettyISK $ orderPriceCents o}
                     <td .numeric>#{orderVolRemaining o}/#{orderVolEntered o} (#{orderMinVolume o})
                     <td .numeric>Range
                     <td .numeric>#{orderDuration o}
                     <td .numeric>#{prettyISK $ orderEscrowCents o}
                     <td>StationName

               <div .panel .panel-default>
               <div .panel-heading>Current Buy Orders:
               <table .table .table-condensed .small>
                 <tr>
                   <th .text-center>Last changed
                   <th .text-center>Item
                   <th .text-center>Price
                   <th .text-center>Quantity (min)
                   <th .text-center>Range
                   <th .text-center>Duration
                   <th .text-center>Escrow
                   <th .text-center>Station
                 $forall (Entity _ o, Single name) <- buyorders
                   <tr>
                     <td>#{showDateTime $ orderIssued $ o}
                     <td>#{name}
                     <td .numeric>#{prettyISK $ orderPriceCents o}
                     <td .numeric>#{orderVolRemaining o}/#{orderVolEntered o} (#{orderMinVolume o})
                     <td .numeric>Range
                     <td .numeric>#{orderDuration o}
                     <td .numeric>#{prettyISK $ orderEscrowCents o}
                     <td>StationName

               |]
             )
