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
import Data.List.Split

eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) -> Gtk.Image -> IO ()
eval ent1 ent2 f img = do
    txt <- Gtk.entryGetText ent1
    result <- try (Gtk.entrySetText ent2 (T.pack (f (T.unpack txt)))) :: IO (Either SomeException ())
    case result of
        Left _ -> do
            Gtk.entrySetText ent2 $ T.pack "Error"
            Gtk.imageSetFromFile img (Just "src/HA.png")
            return ()
        Right _ -> do
            genImage ent2
            Gtk.imageSetFromFile img (Just "src/asd.jpeg")            
            return ()
    return ()

genPoints :: ST.Expression -> [Double] -> [[(Double, Double)]]
genPoints expr s = map (\l -> map (\d -> (d, N.toDouble (ST.calculate (ST.replace d expr)))) l) 
    (splitWhen (\e -> not (ST.isCalculateable (ST.replace e expr))) s) 

genImage :: Gtk.Entry -> IO ()
genImage ent = do
    txt <- Gtk.entryGetText ent
    let expr = ST.makeSyntax $ T.unpack txt
    let myPoints = genPoints expr $ map (\x -> (fromIntegral . round $ x * 10) / 10 ) [0.1,0.2..10.0]

    toFile def "src/asd.jpeg" $ do
        setColors [opaque blue]
        plot (line "" myPoints)
        return ()


main :: IO()
main = do
    _ <- Gtk.init Nothing

    win <- new Gtk.Window [#title := "Haskell-Alpha"]
    #maximize win
    Gtk.setContainerBorderWidth win 10
    _ <- on win #destroy Gtk.mainQuit

    css <- Gtk.cssProviderNew
    let cssFile = "src/boxStyle.css"
    Gtk.cssProviderLoadFromPath css cssFile
    
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10]
    #add win box

    ctx <- Gtk.widgetGetStyleContext box
    Gtk.styleContextAddProvider ctx css 800

    entry <- new Gtk.Entry []
    result <- new Gtk.Entry []

    buttonGrid <- Gtk.gridNew
    Gtk.gridSetColumnSpacing buttonGrid 10
    Gtk.gridSetColumnHomogeneous buttonGrid True

    img <- Gtk.imageNewFromFile ("src/HA.png")
    
    derButton <- new Gtk.Button [#label := "derivate"]
    _ <- on derButton #clicked (eval entry result (show . SM.simplifying . DV.derivate . ST.makeSyntax) img)
    intButton <- new Gtk.Button [#label := "integrate"]
    _ <- on intButton #clicked (eval entry result (show . SM.simplifying . IN.integrate . SM.simplifying . ST.makeSyntax) img)
    _ <- on entry #activate (eval entry result (show . SM.simplifying . ST.makeSyntax) img)

    #add buttonGrid derButton
    #add buttonGrid intButton
    
    #add box entry
    #add box buttonGrid
    #add box result
    #add box img

    #showAll win
    Gtk.main