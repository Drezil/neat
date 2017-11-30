{-# LANGUAGE OverloadedStrings #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
--import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
--import Yesod.Auth.HashDB    (authHashDB)
import Yesod.Auth.OAuth2.EveOnline (oauth2EveScoped,WidgetType(..))
import Data.Time.Format     (parseTimeM)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_neat_css
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_bootstrap_js
            addScript $ StaticR js_neat_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    --authenticate :: Creds master -> HandlerT master IO (AuthenticationResult master)
    authenticate creds =
      runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        now <- liftIO getCurrentTime
        -- extra is charName tokenType and expires
        liftIO $ print extra
        case x of
            Just (Entity uid _) -> do
                                   update uid [UserLastLogin =. now]
                                   return $ Authenticated uid
            Nothing -> case fromExtra of
                       Just (expiry,token,name,cid) -> Authenticated <$> insert User
                                   { userIdent = credsIdent creds
                                   , userName = name
                                   , userCharId = cid
                                   , userPassword = Nothing
                                   , userLastLogin = now
                                   , userTokenExpires = expiry
                                   , userAccessToken = token
                                   , userWalletTimeout = now
                                   , userStandingsTimeout = now
                                   , userSkillTimeout = now
                                   , userBalanceTimeout = now
                                   , userOrderTimeout = now
                                   , userAcc = 0
                                   , userBr = 0
                                   , userBalanceCents = 0
                                   , userStockCents = 0
                                   , userEscrowCents = 0
                                   }
                       Nothing -> return $ ServerError "Problems extracting Access-Token"
        where
          extra = credsExtra creds
          fromExtra = do
                    expires <- getFromExtra "expires" extra
                    token   <- getFromExtra "accessToken" extra
                    name    <- getFromExtra "charName" extra
                    cid     <- getFromExtra "charId" extra >>= readMay
                    expiry  <- parseTimeM True defaultTimeLocale "%FT%X" (unpack expires) :: Maybe UTCTime
                                                  -- %F = YYYY-MM-DD
                                                  -- %X = HH-MM-SS
                    return (expiry,token,name,cid)
          getFromExtra :: Text -> [(Text,Text)] -> Maybe Text
          getFromExtra s = fmap snd . listToMaybe . filter ((==) s . fst)

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ --authBrowserId def
                    --, authHashDB (Just . UniqueUser)
                      oauth2EveScoped "346ee0ad8f974e30be6bf422f40d891b" "kXnZd7f3pmRCvePiBPQcTL2aRcgBHSgPRxc6VNYT" 
                          [ "publicData"
                          , "characterContactsRead"
                          , "characterLocationRead"
                          , "characterWalletRead"
                          , "characterAssetsRead"
                          , "characterIndustryJobsRead"
                          , "characterMarketOrdersRead"
                          , "characterResearchRead"
                          , "characterAccountRead"
                          , "characterContractsRead"
                          , "characterClonesRead"
                          , "characterLoyaltyPointsRead"
                          , "corporationWalletRead"
                          , "corporationAssetsRead"
                          , "corporationIndustryJobsRead"
                          , "corporationMarketOrdersRead"
                          , "corporationStructuresRead"
                          , "remoteClientUI"
                          , "esi-location.read_location.v1"
                          , "esi-skills.read_skills.v1"
                          , "esi-wallet.read_character_wallet.v1"
                          , "esi-wallet.read_corporation_wallet.v1"
                          , "esi-search.search_structures.v1"
                          , "esi-clones.read_clones.v1"
                          , "esi-universe.read_structures.v1"
                          , "esi-assets.read_assets.v1"
                          , "esi-planets.manage_planets.v1"
                          , "esi-ui.open_window.v1"
                          , "esi-ui.write_waypoint.v1"
                          , "esi-markets.structure_markets.v1"
                          , "esi-corporations.read_structures.v1"
                          , "esi-corporations.write_structures.v1"
                          , "esi-characters.read_standings.v1"
                          , "esi-industry.read_character_jobs.v1"
                          , "esi-markets.read_character_orders.v1"
                          , "esi-contracts.read_character_contracts.v1"
                          , "esi-clones.read_implants.v1"
                          , "esi-corporations.track_members.v1"
                          , "esi-wallet.read_corporation_wallets.v1"
                          , "esi-assets.read_corporation_assets.v1"
                          , "esi-corporations.read_blueprints.v1"
                          , "esi-contracts.read_corporation_contracts.v1"
                          , "esi-corporations.read_standings.v1"
                          , "esi-corporations.read_starbases.v1"
                          , "esi-industry.read_corporation_jobs.v1"
                          , "esi-corporations.read_container_logs.v1"
                          , "esi-industry.read_character_mining.v1"
                          , "esi-industry.read_corporation_mining.v1"
                          , "esi-planets.read_customs_offices.v1"
                          , "esi-corporations.read_outposts.v1"
                          ]
                          widget
                    ]
        where
             widget = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/4fSjj56uD6CYwYyus4KmES/4f6385c91e6de56274d99496e6adebab/EVE_SSO_Login_Buttons_Large_Black.png?w=270&h=45">|]

    authHttpManager = getHttpManager
instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
