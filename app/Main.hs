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

writeError :: Gtk.Entry -> SomeException -> IO()
writeError ent _ = Gtk.entrySetText ent $ T.pack "Error"

writeResult :: T.Text -> Gtk.Entry -> (String -> String) -> IO ()
writeResult txt ent f = do
    Gtk.entrySetText ent (T.pack (f (T.unpack txt)))


eval :: Gtk.Entry -> Gtk.Entry -> (String -> String) -> IO ()
eval ent1 ent2 f = do
    txt <- Gtk.entryGetText ent1
    catch (writeResult txt ent2 f) (writeError ent2)
    genImage ent2


am :: ST.Expression -> Double -> IO (Maybe Double)
am expr x = do
    result <- try (return (N.toDouble (ST.calculate (ST.replace x expr)))) :: IO (Either SomeException Double)
    case result of
        Left _ -> return Nothing
        Right n -> return $ Just n

-- signal :: ST.Expression -> [Double] -> [(Double,Double)]
-- signal expr = map (\x -> (x, am expr x))

-- genPoint :: ST.Expression -> Double -> IO (Either Double Char)
-- genPoint expr n = do
--     x <- (Left (catch (am expr n))) _ 
--     return x where
--         handler :: SomeException -> IO (Double)
--         handler _ = return 0

genPoints :: ST.Expression -> [Double] -> IO ([(Double, Double)])
genPoints _ [] = do
    let list = []
    return list
genPoints expr (x:xs) = do
    list <- genPoints expr xs
    y <- am expr x
    case y of
        Nothing -> return list
        Just d -> return $ (x, d) : list


genImage :: Gtk.Entry -> IO ()
genImage ent = do
    txt <- Gtk.entryGetText ent
    let expr = ST.makeSyntax $ T.unpack txt
    -- print ""
    myPoints <- genPoints expr [0.0,0.1..10.0]

    toFile def "src/asd.jpeg" $ do
        setColors [opaque blue]
        plot (line "" ([myPoints]))


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