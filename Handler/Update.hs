{-# LANGUAGE DoAndIfThenElse #-}

module Handler.Update where

import Import
import qualified Eve.Api.Char.WalletTransactions as WT
import qualified Eve.Api.Types as T
import qualified Eve.Api.Char.Standings as ST
import qualified Eve.Api.Char.Skills as SK
import qualified Eve.Api.Char.AccountBalance as BA
import qualified Eve.Api.Char.MarketOrders as MO
import Database.Persist.Sql
import Data.Time.Clock
import Control.Lens.Operators

accountingId :: Int64
accountingId = 16622
brokerRelationId :: Int64
brokerRelationId = 3446

getUpdateR :: Handler Html
getUpdateR = loginOrDo (\(uid,user) -> do
               man <- getHttpManager <$> ask
               apiKey <- runDB $ getBy $ UniqueApiUser uid
               now <- liftIO getCurrentTime
               case apiKey of
                 Nothing -> return ()
                 Just (Entity _ (Api _ k v)) -> do
                   let apidata = T.mkComplete k v (userCharId user)
                   --update skills
                   when (userSkillTimeout user < now) $
                     do
                     skills <- liftIO $ SK.getSkills man apidata
                     case skills of
                       T.QueryResult time' skills' -> runDB $ do
                                         update uid [UserAcc =. findLvl accountingId skills']
                                         update uid [UserBr =. findLvl brokerRelationId skills']
                                         update uid [UserSkillTimeout =. time']
                       _ -> return ()
                   --update standings
                   when (userStandingsTimeout user < now) $
                     do
                     standings <- liftIO $ ST.getStandings man apidata
                     case standings of
                       T.QueryResult time' (_,cstand,fstand) -> runDB $ do
                                         deleteWhere [CorpStandingsUser ==. uid]
                                         deleteWhere [FactionStandingsUser ==. uid]
                                         insertMany_ (migrateCorpStandings uid <$> cstand)
                                         insertMany_ (migrateFactionStandings uid <$> fstand)
                                         update uid [UserStandingsTimeout =. time']
                       _ -> return ()
                   --update transactions
                   when (userWalletTimeout user < now) $
                     do
                     lastid <- runDB $ selectFirst [TransactionUser ==. uid] [Desc TransactionTransId]
                     trans <- case lastid of
                       Just (Entity _ t) -> liftIO $ WT.getWalletTransactionsBackTo man apidata (transactionTransId t)
                       Nothing           -> liftIO $ WT.getWalletTransactionsBackTo man apidata 0
                     case trans of
                       T.QueryResult time' trans' -> runDB $ do
                                                           update uid [UserWalletTimeout =. time']
                                                           insertMany_ (migrateTransaction uid <$> trans')
                       _ -> return ()
                   -- update taxes
                   let sql = "update transaction t \
                           set \
                             fee = 100*(quantity*(price_cents/100)*(0.0100-0.0005*ch.br)/exp(0.1000*COALESCE((select faction_standing from faction_standings where faction_id=c.\"factionID\" and \"user\"=t.\"user\"),0)+0.0400*COALESCE((select corp_standing from corp_standings where corp_id=c.\"corporationID\" and \"user\"=t.\"user\"),0))), \
                             tax = 100*(CASE WHEN t.trans_is_sell THEN quantity*(price_cents/100)*(0.015-(0.0015*ch.acc)) ELSE 0 END) \
                           from \
                             \"staStations\" s \
                             join \"crpNPCCorporations\" c on (s.\"corporationID\" = c.\"corporationID\"),\
                             \"user\" ch \
                           where \
                             t.station_id = s.\"stationID\" and \
                             t.\"user\" = ch.id and \
                             t.fee IS NULL and t.tax IS NULL and \
                             t.no_tax = false and \
                             t.user=?"
                   runDB $ rawExecute sql [toPersistValue uid]
                   -- calculate profits
                   runDB $ do
                     trans <- updateProfits <$> selectList [TransactionUser ==. uid, TransactionInStock !=. 0, TransactionProblematic ==. False] [Asc TransactionDateTime]
                     mapM_ (\(Entity eid t) -> replace eid t) trans
                   let updateProblemSql = "update transaction t set problematic=true where t.trans_is_sell and t.in_stock < 0"
                   runDB $ rawExecute updateProblemSql []
                   --update Balance
                   when (userBalanceTimeout user < now) $
                      do
                     balance <- liftIO $ BA.getAccountBalance man apidata
                     case balance of
                       T.QueryResult time' balance' -> runDB $ do
                                         update uid [UserBalanceCents =. fromIntegral (balance' ^. BA.centbalance)]
                                         update uid [UserBalanceTimeout =. time']
                       _ -> return ()
                   --update stock-worth (cache)
                   let stocksql = "update \"user\" set \
                                   stock_cents = (select sum(in_stock*price_cents) from transaction where \"user\"=\"user\".id and price_cents > 0 and in_stock > 0 and not trans_is_sell)\
                                   where id=?"
                   runDB $ rawExecute stocksql [toPersistValue uid]
                   --get current Orders
                   when (userOrderTimeout user < now) $
                     do
                       orders <- liftIO $ MO.getMarketOrders man apidata
                       case orders of
                         T.QueryResult time' orders' -> runDB $ do
                                         deleteWhere [OrderUser ==. uid]
                                         insertMany_ (migrateOrders uid <$> orders')
                                         update uid [UserOrderTimeout =. time']
                                         --update escrow-worth (cache)
                                         let ordersql = "update \"user\" set \
                                                        escrow_cents = COALESCE((select sum(escrow_cents) from \"order\" where \"user\"=\"user\".id),0) \
                                                        where id=?"
                                         rawExecute ordersql [toPersistValue uid]
                         _ -> return ()
               redirect WalletR
             )

updateProfits :: [Entity Transaction] -> [Entity Transaction]
updateProfits [] = []
updateProfits dat = updateProfits' [] dat
  where
  updateProfits' seen (x@(Entity _ tx):xs) = if transactionTransIsSell tx then
                         let (x',xs') = updateProfits'' x seen
                             updateProfits'' :: Entity Transaction -> [Entity Transaction] -> (Entity Transaction, [Entity Transaction])
                             updateProfits'' o [] = (o,[])
                             updateProfits'' o@(Entity et t) ((Entity cet ct):ts) =
                               if transactionTypeId t == transactionTypeId ct
                                  && transactionInStock ct > 0
                                  && transactionInStock t < 0 then
                                 let m = min (transactionInStock t * (-1)) (transactionInStock ct)
                                     t' = t { transactionInStock = transactionInStock t + m
                                            , transactionProfit  = maybe (Just prof') (\a -> Just (a + prof')) (transactionProfit t)
                                            , transactionSecondsToSell = maybe (Just secs) (\a -> Just ((a*done + secs * m)`div`(done+m))) (transactionSecondsToSell t)
                                            }
                                     ct' = ct {transactionInStock = transactionInStock ct - m}
                                     prof' = (transactionPriceCents t - transactionPriceCents ct) * m
                                     secs = round $ diffUTCTime (transactionDateTime t) (transactionDateTime ct)
                                     done = (transactionQuantity t + transactionInStock t)
                                     (t'',ct'') = updateProfits'' (Entity et t') ts
                                 in
                                   (t'' ,(Entity cet ct'):ct'')
                               else
                                 let
                                   (t'',ct'') = updateProfits'' o ts
                                 in
                                   (t'',(Entity cet ct):ct'')
                         in
                         updateProfits' (xs'++[x']) xs
                       else
                         updateProfits' (seen++[x]) xs
  updateProfits' seen [] = seen

findLvl :: Int64 -> [SK.Skill] -> Int
findLvl sid skills = case find (\(SK.Skill sid' _ _ _) -> sid' == sid) skills of
                     Just (SK.Skill _ _ lvl _) -> lvl
                     Nothing -> 0

migrateCorpStandings :: UserId -> ST.Standing -> CorpStandings
migrateCorpStandings u (ST.Standing cid cname standing) = CorpStandings u cid cname standing

migrateFactionStandings :: UserId -> ST.Standing -> FactionStandings
migrateFactionStandings u (ST.Standing cid cname standing) = FactionStandings u cid cname standing

migrateTransaction :: UserId -> WT.Transaction -> Transaction
migrateTransaction u (WT.Transaction dt tid q tn ti pc ci cn si sn tt tf jti) =
    Transaction u dt tid q (if tis tt then -q else q) tn ti
                (fromIntegral pc) ci cn si sn (tis tt) (tfc tf) jti
                Nothing Nothing Nothing Nothing False False
    where
      tis :: WT.TransactionType -> Bool
      tis WT.Sell = True
      tis WT.Buy = False
      tfc :: WT.TransactionFor -> Bool
      tfc WT.Corporation = True
      tfc WT.Personal = False

migrateOrders :: UserId -> MO.Order -> Import.Order
migrateOrders uid (MO.Order oid cid sid ve vr mv os tid r ak dur esc pric bid iss) =
               Import.Order uid oid cid sid ve vr mv (fromIntegral . fromEnum $ os) tid (fromIntegral . fromEnum $ r) (fromIntegral ak) (fromIntegral dur) (fromInteger esc) (fromInteger pric) (bid == MO.Sell) iss

