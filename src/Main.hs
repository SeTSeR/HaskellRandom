module Main where

import Control.Monad.IO.Class
import Random
import Graphics.UI.Gtk          hiding (fill, rectangle)
import Graphics.UI.Gtk.Builder
import Graphics.Rendering.Cairo

drawRect :: Double -> Double -> Double -> Double -> Render ()
drawRect x y w h = do
    setSourceRGB 0 1 0
    rectangle x y w h
    fill

drawHistogram :: Int -> Int -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> Int -> Render ()
drawHistogram w h f (x1, x2) n m = go f (x1, x2) 1 n
    where
        w'  = fromIntegral w
        h'  = fromIntegral h
        w'' = w'/(fromIntegral n)
        m' = fromIntegral m
        myheight x = (fromIntegral x)*h'/m'
        go f (x1, x2) m n | m==1      = do
            drawRect ((fromIntegral (m-1))*w'') (myheight x1) w'' (myheight x1)
            go f (f (x1, x2)) (m+1) n

        go f (x1, x2) m n | m<=n      = do
             drawRect ((fromIntegral (m-1))*w'') (myheight x2) w'' (myheight x2)
             go f (f (x1, x2)) (m+1) n

        go f (x1, x2) m n | otherwise = return ()

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
    area    <- builderGetObject builder castToDrawingArea "drawingarea1"
    w       <- widgetGetAllocatedWidth area
    h       <- widgetGetAllocatedHeight area
    putStrLn $ research (gen (read sa) (read sb) (read sc) (read sm)) ((read sx1), (read sx2)) (read sm)
    area `on` draw $ (drawHistogram w h (gen (read sa) (read sb) (read sc) (read sm)) ((read sx1), (read sx2)) 100 (read sm))
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
