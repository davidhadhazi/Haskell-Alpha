{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import qualified SyntaxTree as ST
import qualified Simplification as SM
import qualified Derivate as DV
import qualified Integrate as IN
import qualified Number as N

import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo

import Control.Exception

import Graphics.Rendering.Chart.Easy

calc :: (String -> String) -> String -> IO (String)
calc f str = do
    let txt = f str
    return txt

eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) -> IO ()
eval ent1 ent2 f = do
    txt <- Gtk.entryGetText ent1
    
    catch (Gtk.entrySetText ent2 (T.pack (f (T.unpack txt)))) handler where
        handler :: SomeException -> IO ()
        handler _ = Gtk.entrySetText ent2 $ T.pack "Error"

    -- result <- try (calc f (T.unpack txt)) :: IO (Either SomeException String)
    -- case result of
    --     Left _ -> Gtk.entrySetText ent2 $ T.pack "Error"
    --     Right str -> Gtk.entrySetText ent2 $ T.pack str

    -- genImage ent2
    -- set ent1 [#text := ""]


am :: ST.Expression -> Double -> Double
am expr x = N.toDouble $ ST.calculate $ ST.replace x expr

signal :: ST.Expression -> [Double] -> [(Double,Double)]
signal expr = map (\x -> (x, am expr x))

genImage :: Gtk.Entry -> IO ()
genImage ent = do
    txt <- Gtk.entryGetText ent
    let expr = ST.makeSyntax $ T.unpack txt

    toFile def "src/asd.jpeg" $ do
        setColors [opaque blue]
        plot (line "" ([signal expr ([0.0,0.01 .. 10.0])]))


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
    _ <- on intButton #clicked (eval entry result (show . SM.simplifying . IN.integrate . SM.simplifying . ST.makeSyntax))
    _ <- on entry #activate (eval entry result (show . SM.simplifying . ST.makeSyntax))

    
    #add box entry
    #add box derButton
    #add box intButton
    #add box result

    #showAll win
    Gtk.main