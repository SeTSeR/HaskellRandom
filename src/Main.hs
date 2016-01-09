module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

gen :: (Int, Int, Int, Int, [Int], Int) -> [Int]
gen (a, b, c, m, (x:(y:xs)), size) | size == 2 = [x, y] ++ xs
                                   | otherwise = gen (a, b, c, m, ([(a*x+b*y+c) `mod` m, x, y] ++ xs), size-1)

genInt :: Int -> Int -> Int -> Int -> Int -> Int -> Int
genInt a b c m x1 x2 = (a*x1+b*x2+c) `mod` m

research :: Int -> (Int -> Int -> Int) -> String
research m genx = "p: " ++ (show $ period m genx) ++ "\n" ++
		 "pp: " ++ (show $ preperiod m genx (period m genx)) ++ "\n" ++
		 "q: " ++ (show $ quality genx)

period :: Int -> (Int -> Int -> Int) -> Int
period m genx = 0

preperiod :: Int -> (Int -> Int -> Int) -> Int -> Int
preperiod m genx period = 0

quality :: (Int -> Int -> Int) -> Int
quality genx = 0

draw :: Int -> Int -> Int -> DrawingArea -> GC -> [Int] -> IO ()
draw begin width m area gc [] = return () 
draw begin width m area gc (x:xs) = do
    (w, h) <- widgetGetSize area
    window <- widgetGetDrawWindow area
    drawRectangle window gc True begin (h-((x*h) `div` m)) width ((x*h) `div` m)
    draw (begin+width) width m area gc xs

drawGraph :: (DrawingArea, Entry, Entry, Entry, Entry, Entry, Entry) -> IO ()
drawGraph (area, edita, editb, editc,
           editx1, editx2, editm) = do
    sa <- entryGetText edita
    sb <- entryGetText editb
    sc <- entryGetText editc
    sx1 <- entryGetText editx1
    sx2 <- entryGetText editx2
    sm <- entryGetText editm
    (x, y) <- widgetGetSize area
    window <- widgetGetDrawWindow area
    gc <- gcNew window
    gcSetValues gc newGCValues {
        foreground = (Color 0 65535 0)
    }
    draw 0 (x `div` count) (read sm) area gc (take count (gen (read sa, read sb, read sc, read sm, [read sx1, read sx2], constN)))
    putStrLn $ research (read sm) $ genInt (read sa) (read sb) (read sc) (read sm)
    where
        count = 100
        constN = 400

main :: IO ()
main = do
    initGUI
    window <- windowNew
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    buttonOK <- buttonNewWithLabel "OK"
    area <- drawingAreaNew
    labela <- labelNew (Just "a:")
    edita <- entryNew
    labelb <- labelNew (Just "b:")
    editb <- entryNew
    labelc <- labelNew (Just "c:")
    editc <- entryNew
    labelx1 <- labelNew (Just "x1:")
    editx1 <- entryNew
    labelx2 <- labelNew (Just "x2:")
    editx2 <- entryNew
    labelm <- labelNew (Just "m:")
    editm <- entryNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600,
                windowTitle := "Random", containerChild := vbox]
    boxPackStart hbox labela PackGrow 0
    boxPackStart hbox edita PackGrow 0
    boxPackStart hbox labelb PackGrow 0
    boxPackStart hbox editb PackGrow 0
    boxPackStart hbox labelc PackGrow 0
    boxPackStart hbox editc PackGrow 0
    boxPackStart hbox labelx1 PackGrow 0
    boxPackStart hbox editx1 PackGrow 0
    boxPackStart hbox labelx2 PackGrow 0
    boxPackStart hbox editx2 PackGrow 0
    boxPackStart hbox labelm PackGrow 0
    boxPackStart hbox editm PackGrow 0
    boxPackStart vbox area PackGrow 0
    boxPackStart vbox hbox PackNatural 0
    boxPackStart vbox buttonOK PackNatural 0
    onClicked buttonOK $ drawGraph (area, edita, editb, editc, editx1, editx2, editm)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
