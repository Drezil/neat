{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Settings where

import Import
import qualified Network.HTTP.Conduit as HTTP
import Text.XML.Lens
import Data.Bits
import Text.XML (parseText)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy.Char8 as B

getSettingsR :: Handler Html
getSettingsR = loginOrDo $ (\(uid,user) -> do
               apiKey <- runDB $ getBy $ UniqueApiUser uid
               (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 authFormLayout (authKeyForm (entityVal <$> apiKey) uid)
               man <- getHttpManager <$> ask
               validKey <- case apiKey of
                             Just (Entity _ key) -> liftIO $ checkApiKey key man
                             Nothing -> return False
               insertionWidget <- return Nothing :: Handler (Maybe Widget)
               loginLayout user $(widgetFile "settings")
               )


postSettingsR :: Handler Html
postSettingsR = loginOrDo $ (\(uid,user) -> do
                apiKey <- runDB $ getBy $ UniqueApiUser uid
                ((result,formWidget),formEnctype) <- runFormPost $ renderBootstrap3 authFormLayout (authKeyForm (entityVal <$> apiKey) uid)
                (success, msg) <- case result of
                  FormSuccess api  -> do mapi <- runDB $ getBy $ UniqueApiUser uid
                                         case mapi of
                                           Just (Entity aid _) -> runDB $ replace aid api
                                           Nothing -> runDB $ insert_ api
                                         return (True,[whamlet|Successful inserted Key|])
                  FormFailure errs -> return (False,[whamlet|Error:<br>#{concat $ intersperse "<br>" errs}|])
                  FormMissing      -> return (False,[whamlet|Error: No such Form|])
                apiKey' <- runDB $ getBy $ UniqueApiUser uid
                man <- getHttpManager <$> ask
                validKey <- case apiKey' of
                              Just (Entity _ key) -> liftIO $ checkApiKey key man
                              Nothing -> return False
                insertionWidget <- return . Just $ [whamlet|
$if success
  <div class="alert alert-success" role="alert">^{msg}
$else
  <div class="alert alert-danger" role="alert">^{msg}
|] :: Handler (Maybe Widget)
                loginLayout user $(widgetFile "settings")
                )

checkApiKey :: Api -> Manager -> IO Bool
checkApiKey (Api _ key code) manager = do
    url <- parseUrl $ "https://api.eveonline.com/account/APIKeyInfo.xml.aspx?keyID="++(show key)++"&vCode="++(unpack code)
    response <- HTTP.httpLbs url manager
    xml' <- return . parseText def . T.pack . B.unpack . responseBody $ response
    case xml' of
      Left _ -> return False
      Right xml -> do
        accessMasks <- return $ xml ^.. root . el "eveapi" ./ el "result" ./ el "key" . attribute "accessMask"
        case headMay accessMasks >>= liftM unpack >>= readMay of
          Just am -> return $ am .&. 132648971 == (132648971 :: Integer)
          _ -> return False
  `catch` (\ (_ :: HttpException) -> return False)

authKeyForm :: Maybe Api -> Key User -> AForm Handler Api
authKeyForm ma u = Api
             <$> pure u
             <*> areq intField (withPlaceholder "keyID" "keyID") (apiKeyID <$> ma)
             <*> areq textField (withPlaceholder "vCode" "vCode") (apiVCode <$> ma)
             <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

authFormLayout :: BootstrapFormLayout
authFormLayout = BootstrapHorizontalForm (ColLg 0) (ColLg 1) (ColLg 0) (ColLg 11)
