module Handler.Wallet where

import Import

getWalletR :: Handler Html
getWalletR = do
             defaultLayout $ [whamlet|
             <h1>Transactions in the last xx hours

             <h1>Statistices for the last xx days
             
             |] 


