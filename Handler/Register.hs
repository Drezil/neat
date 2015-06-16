module Handler.Register where

import Import
import Yesod.Form.Bootstrap3
import Handler.Home (getHomeR)

getRegisterR :: Handler Html
getRegisterR = do
               (registerWidget, registerEnctype) <- generateFormPost registerForm
               defaultLayout $ do
                 setTitle "Register"
                 [whamlet|
                      <h1>Register
                      <form method=post action=@{RegisterR} enctype=#{registerEnctype}>
                          ^{registerWidget}
                 |]

postRegisterR :: Handler Html
postRegisterR = do
                ((result,registerWidget), registerEnctype) <- runFormPost registerForm
                let again err = defaultLayout $ do
                        setTitle "Register"
                        [whamlet|
                        <div class="alert alert-danger fade in"><strong>Error:</strong> #{err}
                        <h1>Register
                        <form method=post action=@{RegisterR} enctype=#{registerEnctype}>
                          ^{registerWidget}
                  |]
                case result of
                  FormSuccess (user,mail) -> do
                        _ <- runDB $ do
                            uid <- insert user
                            insert $ Email mail uid Nothing
                        getHomeR
                  FormFailure (err:_) -> again err
                  _ -> again "Invalid input"


registerForm :: Html -> MForm Handler (FormResult (User,Text), Widget)
registerForm extra = do
           (nameRes, nameView) <- mreq textField ((withAutofocus . withPlaceholder "Username") (bfs ("Username" :: Text))) Nothing
           (pwRes, pwView) <- mreq passwordField (bfs ("Password" :: Text)) Nothing
           (pwcRes, pwcView) <- mreq passwordField (bfs ("Confirm password" :: Text)) Nothing
           (emailRes, emailView) <- mreq emailField (withPlaceholder "User@mail" (bfs ("Email" :: Text))) Nothing
           time <- lift $ liftIO getCurrentTime
           let confirmRes = case pwRes of
                             FormSuccess x -> case pwcRes of
                                             FormSuccess y -> if x == y then FormSuccess x else FormFailure ["Passwords did not match"]
                                             a -> a
                             a -> a
           let registerRes = (,) <$> (User <$> nameRes <*> (Just <$> confirmRes) <*> (FormSuccess time))
                                 <*> emailRes
           let widget = [whamlet|
                      #{extra}
                      <p>
                         Username ^{fvInput nameView}
                      <p>
                         Password ^{fvInput pwView}
                      <p>
                         Confirm password ^{fvInput pwcView}
                      <p>
                         Email ^{fvInput emailView}
                      <input type=submit value="register">
                    |]
           return (registerRes, widget)
