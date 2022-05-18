module OurStuff.App where

app :: Spock ()
app = do
    get root $ text "yo"
    get countersPath increaseAndShowCounter


