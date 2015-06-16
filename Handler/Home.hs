module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout $ do
        setTitle "NEAT"
        [whamlet|
            <h1>
                Welcome to NEAT.
            <div>
                Current Auth-ID: #{show maid}.
            <div>
                $maybe u <- maid
                    <p>
                       Data: #{show u}<br>
                       <a href=@{AuthR LogoutR}>Logout
                $nothing
                    <p>
                        <a href=@{AuthR LoginR}>Login
                    <p>
                        <a href=@{RegisterR}>Register Account
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
