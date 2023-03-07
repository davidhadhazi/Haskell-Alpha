{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import qualified SyntaxTree as ST

eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) ->IO ()
eval ent1 ent2 f = do
    txt <- Gtk.entryGetText ent1
    let txt' = show $ ST.calculate $ ST.makeSyntax $ T.unpack txt
    _ <- Gtk.entrySetText ent2 $ T.pack txt'
    set ent1 [#text := ""]

main :: IO()
main = do
    _ <- Gtk.init Nothing

    win <- new Gtk.Window [#title := "Haskell-Alpha"]
    #maximize win
    _ <- on win #destroy Gtk.mainQuit

    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10]
    #add win box

    entry <- new Gtk.Entry []

    result <- new Gtk.Entry []

    button <- new Gtk.Button [#label := "calculate"]
    _ <- on button #clicked (eval entry result (show))
    
    #add box entry
    #add box button
    #add box result

    #showAll win
    Gtk.main