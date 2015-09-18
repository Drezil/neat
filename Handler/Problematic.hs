module Handler.Problematic where

import Import

problematicDaysInterval :: [Int]
problematicDaysInterval = [1,2,7,14,31]

getProblematicR :: Handler Html
getProblematicR = getProblematicDaysR 1

getProblematicDaysR :: Int -> Handler Html
getProblematicDaysR days = loginOrDo (\(uid,user) -> do
        let rawSQL = "select \
                      max(date_time), trans_for_corp, trans_is_sell, type_id, type_name, sum(quantity) as quantity, \
                      sum(in_stock) as in_stock, sum(price_cents) as price_cents, station_id, station_name \
                      from \
                      transaction \
                      where \
                      \"user\"=? and CURRENT_TIMESTAMP - date_time < INTERVAL '? day' and problematic \
                      group by type_id, trans_for_corp, trans_is_sell, type_name, station_id, station_name \
                      order by max(date_time) desc"
        loginLayout user $ [whamlet|
        Not yet implemented.
        |]
        )
