module OurStuff.Handlers where

err :: MonadIO m => String -> m a
err msg = liftIO (ioError (userError msg))


increaseAndShowCounter :: String -> SpockAction _ _ _ _
increaseAndShowCounter name = do
    count <-
        S.runQuery $ \connection -> do
            [Only c] <-
                PSQL.query @_ @(Only Int)
                    connection
                    "\
                    \ INSERT INTO counters (name, count) \
                    \ VALUES (?, 1) \
                    \ ON CONFLICT (name) \
                    \ DO UPDATE SET count = counters.count + 1 \
                    \ RETURNING count"
                    (Only name)
            pure c

    hostname <- decodeUtf8 . guessApproot <$> S.request

    text $
        T.unlines
            [ "yooo " <> T.pack (show count)
            , "Go to " <> hostname <> S.renderRoute countersPath name <> "!"
            ]
