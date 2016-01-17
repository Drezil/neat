module Handler.Item where

import Import
import Yesod.Default.Util (widgetFileReload)

itemsPerPage :: Int
itemsPerPage = 100

getItemR :: Int64 -> Handler Html
getItemR transactionTypeId = getItemPagedR transactionTypeId 1

getItemPagedR :: Int64 -> Int -> Handler Html
getItemPagedR tid page
   | page < 1 = getItemPagedR tid 1
   | otherwise = loginOrDo (\(uid,user) -> do
     items <- runDB $ selectList [TransactionTypeId ==. tid] [Desc TransactionDateTime, LimitTo itemsPerPage, OffsetBy (itemsPerPage*(page-1))]
     total <- runDB $ count [TransactionTypeId ==. tid]
     let offset = itemsPerPage * page
     let maxPages = total `div` itemsPerPage
     let paginatePages = [1..maxPages+1]
     loginLayout user $ $(widgetFileReload def "item")
   )

