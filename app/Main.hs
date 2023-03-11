{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import qualified SyntaxTree as ST
import qualified Simplification as SM
import qualified Derivate as DV

eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) ->IO ()
eval ent1 ent2 f = do
    txt <- Gtk.entryGetText ent1
    let txt' = f $ T.unpack txt
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

    derButton <- new Gtk.Button [#label := "derivate"]
    _ <- on derButton #clicked (eval entry result (show . SM.simplifying . DV.derivate . ST.makeSyntax))
    intButton <- new Gtk.Button [#label := "integrate"]
    _ <- on intButton #clicked (eval entry result (show . SM.simplifying . DV.derivate . ST.makeSyntax))
    _ <- on entry #activate (eval entry result (show . SM.simplifying . ST.makeSyntax))
    
    #add box entry
    #add box derButton
    #add box intButton
    #add box result

    #showAll win
    Gtk.main