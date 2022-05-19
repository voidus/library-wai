{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module OurStuff.Paths where

import Web.Spock


counters = "counters" <//> var @String


items :: Path '[] _
items = "items"
