module Handler.Home where

import Import

loginOrElse :: ((Key User, User) -> Handler Html) -> Handler Html -> Handler Html
loginOrElse cont contElse = do
                 maid <- maybeAuthId
                 muid <- case maid of
                         Just uid -> fmap ((,) uid) <$> runDB (get uid)
                         Nothing -> return Nothing
                 case muid of
                      Nothing -> contElse
                      Just (uid,u) -> cont (uid,u)

getHomeR :: Handler Html
getHomeR = do
    loginOrElse getLoggedIn getNotLoggedIn


getLoggedIn :: (Key User, User) -> Handler Html
getLoggedIn (uid, user) = do
    loginLayout user $ [whamlet|
             <h1>Welcome back, #{userName user}.
             <p>Current Balance: #{prettyISK $ userBalanceCents user} ISK.
             <p>Current Stock Worth: ...
             <p>Current total Worth: ...
             <p>Profit in the last 7 days: ...
             |]



getNotLoggedIn :: Handler Html
getNotLoggedIn = do
    defaultLayout $ do
        setTitle "NEAT"
        [whamlet|
            <h1>Welcome to NEAT.
            <div>Here we should present features, images and other stuff to get people hooked.
        |]
{-
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")-}

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ [whamlet|
    <h1>nothing to see here. Stuff coming soon (tm).
    |]
{-    ((result, loginWidget), loginEnctype) <- runFormPost loginForm
    let loginfail err = defaultLayout $ do
        setTitle "NEAT"
        [whamlet|
            <h1>
                Welcome to NEAT.
            <div>
                <div class="alert alert-danger fade in">#{err}
                Login
                <form method=post action=@{HomeR} enctype=#{loginEnctype}>
                    ^{loginWidget}
                    <button>Submit
               <a href=@{RegisterR}>Register Account
        |]
    case result of
      FormSuccess (u,pw) -> do
                       login <- runDB $ selectFirst [UserIdent ==. u, UserPassword ==. (Just pw)] []
                       case login of
                         Nothing -> loginfail ("wrong username or password" :: Text)
                         Just (Entity _ (User name _ _)) ->
                                defaultLayout $ do [whamlet|<h1>Hello #{name}|]
      _ -> loginfail ("wrong username or password" :: Text)-}


{-loginForm :: Form (Text, Text)
loginForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField ((withAutofocus . withPlaceholder "Username") (bfs ("Username" :: Text))) Nothing
    <*> areq passwordField (bfs ("Password" :: Text)) Nothing-}
