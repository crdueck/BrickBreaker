{-# LANGUAGE FlexibleInstances #-}
import BrickBreaker
import Graphics.UI.SDL

import Prelude hiding (flip)
import Control.Monad
import qualified Data.Map as M

class Drawable a where
    draw :: Surface -> a -> IO ()

instance Drawable (Position, Pixel) where
    draw screen ((x, y), pix) =
        void $ fillRect screen (Just $ Rect x y 2 2) pix

instance Drawable Particle where
    draw screen (Particle x y _ _ pix) =
        draw screen ((x, y), pix)

instance Drawable Paddle where
    draw screen (Paddle x w h) =
        void $ fillRect screen (Just $ Rect x y w h) (Pixel 0x00FF00)
        where y = height - h

instance Drawable GameState where
    draw screen (GS ps bs) = do
        mapM_ (draw screen) ps
        mapM_ (draw screen) $ M.toList bs

erase :: Surface -> Particle -> IO Bool
erase screen (Particle x y _ _ _) =
    fillRect screen (Just $ Rect x y 2 2) (Pixel 0x000000)

drawGame :: Surface -> Paddle -> GameState -> IO ()
drawGame screen pd gs = do
    void $ fillRect screen Nothing (Pixel 0x000000)
    draw screen gs
    draw screen pd
    flip screen

updateWorld :: Surface -> Paddle -> GameState -> IO ()
updateWorld screen pd@(Paddle _ w h) gs = do
    ticks     <- getTicks
    drawGame screen pd gs
    pumpEvents
    (x, _, _) <- getMouseState
    let newPD = Paddle (min x (width - w)) w h
        newGS = updateGame newPD gs
    ticks'    <- getTicks
    let dt  = ticks' - ticks
    when (dt < fromIntegral secsPerFrame) $
        delay $ fromIntegral secsPerFrame - dt
    unless (gameOver gs) (updateWorld screen newPD newGS)

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode width height depth [SWSurface]
    setCaption "BrickBreakerSDL" "BrickBreakerSDL"
    showCursor False
    drawGame screen initPaddle initGS
    delay 1000
    updateWorld screen initPaddle initGS

initParticle = Particle (width `quot` 2) (height `quot` 2) 4 6 (Pixel 0xFFFFFF)
initPaddle   = Paddle (width `quot` 2) 60 5
initGS       = GS [initParticle] genBlock

testGS       = GS testPts genBlock
testPts      = take 20 [Particle x 20 1 1 (Pixel 0xffffff) | x <- [0,30..]]
