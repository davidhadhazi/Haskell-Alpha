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
import Text.Read

onError :: Gtk.Entry -> Gtk.Image -> IO ()
onError entry img = do
    Gtk.entrySetText entry "Error"
    Gtk.imageSetFromFile img (Just "src/HA.png")

eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) -> Gtk.Image -> IO ()
eval ent1 ent2 f img = do
    txt <- Gtk.entryGetText ent1
    result <- try (Gtk.entrySetText ent2 (T.pack (f (T.unpack txt)))) :: IO (Either SomeException ())
    case result of
        Left _ -> do
            onError ent2 img
        Right _ -> do
            genImage ent2
            Gtk.imageSetFromFile img (Just "src/graph.jpeg")            

genPoints :: ST.Expression -> [Double] -> [[(Double, Double)]]
genPoints expr s = map (\l -> map (\d -> (d, N.toDouble (ST.calculate (ST.replace d expr)))) l) 
    (splitWhen (\e -> not (ST.isCalculateable (ST.replace e expr))) s) 

genImage :: Gtk.Entry -> IO ()
genImage ent = do
    txt <- Gtk.entryGetText ent
    let expr = ST.makeSyntax $ T.unpack txt
    let myPoints = genPoints expr $ map (\x -> x / 10.0 ) [0..100]

    toFile def "src/graph.jpeg" $ do
        setColors [opaque blue]
        plot (line "" myPoints)
        return ()

onIntClicked :: Gtk.Entry -> Gtk.Entry -> Gtk.Entry -> Gtk.Entry -> Gtk.Image -> IO ()
onIntClicked startEntry endEntry exprEntry resultEntry img = do
    startTxt <- Gtk.entryGetText startEntry
    endTxt <- Gtk.entryGetText endEntry

    let startStr = T.unpack startTxt
    let endStr = T.unpack endTxt

    if startStr == "" && endStr == "" then
        (eval exprEntry resultEntry (show . SM.simplifying . IN.integrate . SM.simplifying . ST.makeSyntax) img) else
            do
                let (start, end) = (readMaybe startStr :: Maybe Double, readMaybe endStr :: Maybe Double)
                case (start, end) of
                    (Just st, Just en) -> eval exprEntry resultEntry (\str -> show (IN.definite_integrate (ST.makeSyntax str) st en)) img
                    _ -> onError resultEntry img

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
    
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #expand := True]
    ctx <- Gtk.widgetGetStyleContext box
    Gtk.styleContextAddProvider ctx css 800
    #add win box

    entryBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 5, #expand := False]
    #setHalign entryBox Gtk.AlignFill
    #setValign entryBox Gtk.AlignCenter
    #add box entryBox

    startEndBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 5]
    #add entryBox startEndBox

    endEntry <- new Gtk.Entry []
    #add startEndBox endEntry
    startEntry <- new Gtk.Entry []
    #add startEndBox startEntry

    entry <- new Gtk.Entry []
    Gtk.widgetSetValign entry Gtk.AlignCenter
    Gtk.boxPackEnd entryBox entry True True 5

    buttonGrid <- Gtk.gridNew
    Gtk.gridSetColumnSpacing buttonGrid 10
    Gtk.gridSetColumnHomogeneous buttonGrid True
    #add box buttonGrid

    derButton <- new Gtk.Button [#label := "derivate"]
    #add buttonGrid derButton
    intButton <- new Gtk.Button [#label := "integrate"]
    #add buttonGrid intButton

    result <- new Gtk.Entry []
    #add box result

    emptyLabel <- new Gtk.Label []
    #add box emptyLabel

    img <- Gtk.imageNewFromFile ("src/HA.png")
    #add box img
    
    _ <- on entry #activate (eval entry result (show . SM.simplifying . ST.makeSyntax) img)
    _ <- on derButton #clicked (eval entry result (show . SM.simplifying . DV.derivate . ST.makeSyntax) img)
    _ <- on intButton #clicked (onIntClicked startEntry endEntry entry result img)

    #showAll win
    Gtk.main