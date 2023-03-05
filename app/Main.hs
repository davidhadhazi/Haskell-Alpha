{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
-- import Tokens
-- import SyntaxTree
-- import Number
-- import Derivate
-- import Simplification

main :: IO()
main = do
    _ <- Gtk.init Nothing

    win <- new Gtk.Window [#title := "Haskell-Alpha"]
    _ <- on win #destroy Gtk.mainQuit
    #showAll win

    Gtk.main