module Main where

import Data.Vector as V         (replicate, (//), (!), length, head, tail)
import Control.Monad.IO.Class
import Random
import Graphics.UI.Gtk          hiding (fill, rectangle)
import Graphics.UI.Gtk.Builder
import Graphics.Rendering.Cairo

drawRHistogram :: Int -> Int -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> Render ()
drawRHistogram w h f (x1, x2) m = setSourceRGB 1 1 1 >> rectangle 0 0 w' h' >> fill >> go 1 (go1 f (x1, x2) 400 m (V.replicate 400 0))
    where
        h'  = fromIntegral h
        w'  = fromIntegral w
        w'' = w'/20
        m'  = fromIntegral m
        myheight x = (fromIntegral x)*h'/m'
        get x1 m = (x1*20) `div` m
        go1 f (x1, x2) n m arr | n==1      = arr
                               | n==400    = go1 f (f (x1, x2)) (n-1) m (arr // [((get x1 m), arr!(get x1 m))])
                               | otherwise = go1 f (f (x1, x2)) (n-1) m (arr // [((get x2 m), arr!(get x2 m))])
        drawRect x y w h = do
            setSourceRGB 0 0 1
            rectangle x y w h
            fill
        go x arr | (V.length arr) == 0 = return ()
                 | otherwise           = do
                drawRect ((fromIntegral (x-1))*w'') (h' - myheight (V.head arr)) w'' (myheight (V.head arr))
                go (x+1) (V.tail arr)

drawHistogram :: Int -> Int -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> Int -> Render ()
drawHistogram w h f (x1, x2) n m = setSourceRGB 1 1 1 >> rectangle 0 0 w' h' >> fill >> go f (x1, x2) 1 n
    where
        w'  = fromIntegral w
        h'  = fromIntegral h
        w'' = w'/(fromIntegral n)
        m'  = fromIntegral m
        myheight x = (fromIntegral x)*h'/m'
        drawRect x y w h = do
            setSourceRGB 0 1 0
            rectangle x y w h
            fill
        go f (x1, x2) m n | m==1      = do
            drawRect ((fromIntegral (m-1))*w'') (h' - myheight x1) w'' (myheight x1)
            go f (f (x1, x2)) (m+1) n

        go f (x1, x2) m n | m<=n      = do
             drawRect ((fromIntegral (m-1))*w'') (h' - myheight x2) w'' (myheight x2)
             go f (f (x1, x2)) (m+1) n

        go f (x1, x2) m n | otherwise = return ()

recalc :: Builder -> IO ()
recalc builder = do
    let parseText label = do
            entry <- builderGetObject builder castToEntry label
            text  <- entryGetText entry
            return $ read text
        labels = ["a", "b", "c", "m", "x1", "x2"]
    [a, b, c, m, x1, x2] <- mapM parseText ["entry" ++ l | l <- labels]
    let getAreas label = do
            area <- builderGetObject builder castToDrawingArea label
            w    <- widgetGetAllocatedWidth area
            h    <- widgetGetAllocatedHeight area
            return (area, w, h)
        labels = ["1", "2"]
    [(area1, w1, h1), (area2, w2, h2)] <- mapM getAreas ["drawingarea" ++ l | l <- labels]
    area1 `on` draw $ (drawHistogram w1 h1 (gen a b c m) (x1, x2) 100 m)
    area2 `on` draw $ (drawRHistogram w2 h2 (gen a b c m) (x1, x2) m)
    putStrLn $ research (gen a b c m) (x1, x2) m
    widgetQueueDraw area1
    widgetQueueDraw area2

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
