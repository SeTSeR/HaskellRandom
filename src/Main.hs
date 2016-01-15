module Main where

import Control.Monad.IO.Class
import Random
import Graphics.UI.Gtk          hiding (fill, rectangle)
import Graphics.UI.Gtk.Builder
import Graphics.Rendering.Cairo

toCortage :: [Int] -> Generator
toCortage [a, b, c, m, x1, x2] = (a, b, c, m, x1, x2)

research :: Generator -> String
research gener = "p: "  ++ (show $ period gener)    ++ "\n" ++
                 "pp: " ++ (show $ preperiod gener) ++ "\n" ++
                 "q: "  ++ (show $ quality gener 19)

drawHistogram :: Int -> Int -> Generator -> (Int, Int) -> Double -> Int -> Render ()
drawHistogram w h (a, b, c, m, x1, x2) (currx1, currx2) currx count | count == 1 = do
                                                                    setSourceRGB 0 1 0
                                                                    rectangle currx (h'-currx2'/m'*h') (w'/100.0) (currx2'/m'*h')
                                                                    fill
                                                                    where
                                                                        h' = fromIntegral h
                                                                        w' = fromIntegral w
                                                                        m' = fromIntegral m
                                                                        currx2' = fromIntegral currx2

drawHistogram w h (a, b, c, m, x1, x2) (currx1, currx2) currx count = do
              setSourceRGB 0 1 0
              rectangle currx (h'-currx2'/m'*h') (w'/100.0) (currx2'/m'*h')
              fill
              drawHistogram w h (a, b, c, m, x1, x2) (gen (a, b, c, m, x1, x2) (currx1, currx2)) (currx + (w'/100.0)) (count-1)
              where
                h'      = fromIntegral h
                w'      = fromIntegral w
                m'      = fromIntegral m
                currx2' = fromIntegral currx2

recalc :: Builder -> IO ()
recalc builder = do
                 entrya  <- builderGetObject builder castToEntry "entrya"
                 entryb  <- builderGetObject builder castToEntry "entryb"
                 entryc  <- builderGetObject builder castToEntry "entryc"
                 entrym  <- builderGetObject builder castToEntry "entrym"
                 entryx1 <- builderGetObject builder castToEntry "entryx1"
                 entryx2 <- builderGetObject builder castToEntry "entryx2"
                 sa      <- entryGetText entrya
                 sb      <- entryGetText entryb
                 sc      <- entryGetText entryc
                 sm      <- entryGetText entrym
                 sx1     <- entryGetText entryx1
                 sx2     <- entryGetText entryx2
                 putStrLn $ research ((read sa), (read sb), (read sc), (read sm), (read sx1), (read sx2))
                 area    <- builderGetObject builder castToDrawingArea "drawingarea1"
                 w <- widgetGetAllocatedWidth area
                 h <- widgetGetAllocatedHeight area
                 area `on` draw $ (drawHistogram w h ((read sa), (read sb), (read sc), (read sm), (read sx1), (read sx2)) ((read sa), (read sb)) 0 100)
                 widgetQueueDraw area

main :: IO ()
main = do
        initGUI
        builder <- builderNew
        builderAddFromFile builder "forms/HaskellRandom.glade"
        window   <- builderGetObject builder castToWindow "window1"
        window `on` deleteEvent $ liftIO mainQuit >> return False
        buttonOK <- builderGetObject builder castToButton "button1"
        buttonOK `on` buttonActivated $ recalc builder
        widgetShowAll window
        mainGUI
