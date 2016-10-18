{-# LANGUAGE OverloadedStrings #-}
module Server where
  
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (sendFile "public/index.html") <|>
  dir "public" (serveDirectory "public") <|>
  dir "node_modules" (serveDirectory "node_modules")
