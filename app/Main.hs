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
import Graphics.Rendering.Chart.Backend.Cairo

import Control.Exception
import Graphics.Rendering.Chart.Easy
import Data.List.Split

-- If something went wrong it tells the user
onError :: Gtk.Entry -> Gtk.Image -> String -> IO ()
onError entry img msg = do
    Gtk.entrySetText entry $ T.pack msg
    Gtk.imageSetFromFile img (Just "src/HA.png")

-- Evaluate the result from the entries and the task
eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) -> Gtk.Image -> IO ()
eval ent1 ent2 f img = do
    txt <- Gtk.entryGetText ent1
    result <- try (Gtk.entrySetText ent2 (T.pack (f (T.unpack txt)))) :: IO (Either SomeException ())
    case result of
        Left e -> onError ent2 img $ head $ splitWhen (== '\n') $ show e
        Right _ -> do
            genImage ent2
            Gtk.imageSetFromFile img (Just "src/graph.png")
            Gtk.imageSetPixelSize img 10           

-- Generates points for the chart
genPoints :: ST.Expression -> [Double] -> [[(Double, Double)]]
genPoints expr s = map (\l -> map (\d -> (d, N.toDouble (ST.calculate (ST.replace d expr)))) l) 
    (splitWhen (\e -> not (ST.isCalculateable (ST.replace e expr))) s) 

-- Generates the image from the result
genImage :: Gtk.Entry -> IO ()
genImage ent = do
    txt <- Gtk.entryGetText ent
    let expr = ST.makeSyntax $ T.unpack txt
    let myPoints = genPoints expr $ map (\x -> x / 10.0 ) [0..100]

    saveResult <- try (toFile (FileOptions (801, 550) PNG) "src/graph.png" (do
        setColors [opaque blue]
        plot (line "" myPoints)
        return ())) :: IO (Either SomeException ())

    case saveResult of
        Left _ -> return ()
        Right _ -> return ()

-- Procedure of the integration
onIntClicked :: Gtk.Entry -> Gtk.Entry -> Gtk.Entry -> Gtk.Entry -> Gtk.Image -> IO ()
onIntClicked startEntry endEntry exprEntry resultEntry img = do
    startTxt <- Gtk.entryGetText startEntry
    endTxt <- Gtk.entryGetText endEntry

    let startStr = T.unpack startTxt
    let endStr = T.unpack endTxt

    -- Checks it is either definite or indefinite integration
    if startStr == "" && endStr == "" then
        -- Indefinite integration
        (eval exprEntry resultEntry (show . SM.simplifying . IN.integrate . SM.simplifying . ST.makeSyntax) img) else
            do
                -- Definite integration
                interval <- try (return (ST.calculate (ST.makeSyntax startStr), ST.calculate (ST.makeSyntax endStr))) :: IO (Either SomeException (N.Number', N.Number'))
                case interval of
                    Left e -> onError resultEntry img $ head $ splitWhen (== '\n') $ show e
                    Right (start, end) -> eval exprEntry resultEntry (\str -> show (IN.definite_integrate (SM.simplifying (ST.makeSyntax str)) start end)) img
main :: IO()
main = do
    -- Gtk initialization
    _ <- Gtk.init Nothing

    win <- new Gtk.Window [#title := "Haskell-Alpha"]
    #maximize win
    Gtk.setContainerBorderWidth win 10
    _ <- on win #destroy Gtk.mainQuit

    -- Css Loading
    css <- Gtk.cssProviderNew
    cssResult <- try (Gtk.cssProviderLoadFromPath css "src/boxStyle.css") :: IO (Either SomeException ())
    case cssResult of
        Left _ -> return ()
        Right _ -> return ()
    
    -- Main box
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #expand := True]
    ctx <- Gtk.widgetGetStyleContext box
    Gtk.styleContextAddProvider ctx css 1
    #add win box

    -- Entries for the input data
    entryBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 5, #expand := False]
    #setHalign entryBox Gtk.AlignFill
    #setValign entryBox Gtk.AlignCenter
    #add box entryBox

    startEndBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 5]
    #add entryBox startEndBox

    -- Interval entries
    endEntry <- new Gtk.Entry []
    Gtk.entrySetText endEntry "`End of interval`"
    #add startEndBox endEntry
    startEntry <- new Gtk.Entry []
    Gtk.entrySetText startEntry "`Start of interval`"
    #add startEndBox startEntry

    -- Input entry
    entry <- new Gtk.Entry []
    Gtk.entrySetText entry "`Please write a valid syntax`"
    Gtk.widgetSetValign entry Gtk.AlignCenter
    Gtk.boxPackEnd entryBox entry True True 5

    -- Grid fo both buttons
    buttonGrid <- Gtk.gridNew
    Gtk.gridSetColumnSpacing buttonGrid 10
    Gtk.gridSetColumnHomogeneous buttonGrid True
    #add box buttonGrid

    derButton <- new Gtk.Button [#label := "Derivate"]
    #add buttonGrid derButton
    intButton <- new Gtk.Button [#label := "Integrate"]
    #add buttonGrid intButton

    -- Entry of the result
    result <- new Gtk.Entry []
    #add box result

    -- Image
    img <- Gtk.imageNewFromFile ("src/HA.png")
    #add box img
    
    -- Infobutton on the bottom of the page
    infoButton <- new Gtk.Button [#label := "Info"]
    _ <- on infoButton #clicked (do 
        infoWin <- new Gtk.Window [#title := "Info"]
        infoBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
        #add infoWin infoBox
        ctxInfo <- Gtk.widgetGetStyleContext infoBox
        Gtk.styleContextAddProvider ctxInfo css 1
        textLabel <- Gtk.labelNew Nothing
        fileResult <- try (readFile "src/infotext.txt") :: IO (Either SomeException (String))
        txt <- case fileResult of
            Left _ -> return "Missing file"
            Right s -> return s
        Gtk.labelSetText textLabel $ T.pack txt
        #add infoBox textLabel
        #showAll infoWin
        )
    #add box infoButton
    
    -- Buttoncommands
    _ <- on entry #activate (eval entry result (show . SM.simplifying . ST.makeSyntax) img)
    _ <- on derButton #clicked (eval entry result (show . SM.simplifying . DV.derivate . ST.makeSyntax) img)
    _ <- on intButton #clicked (onIntClicked startEntry endEntry entry result img)

    -- Gtk start
    #showAll win
    Gtk.main