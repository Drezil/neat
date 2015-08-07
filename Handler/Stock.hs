module Handler.Stock where

import Import

getStockR :: Handler Html
getStockR = loginOrDo (\(uid,user) -> do
              items <- runDB $ selectList [TransactionUser ==. uid, TransactionInStock >. 0] [Asc TransactionTypeName]
              defaultLayout $ [whamlet|
             <h1>Current Stock
             <table .table>
               <tr>
                 <th>Item name
                 <th>Quantity
                 <th>Buy Price
               $forall Entity _ t <- items
                 <tr>
                   <td>#{transactionTypeName t}
                   <td>#{transactionInStock t}
                   <td>#{transactionPriceCents t}
             |]
            )
