module BrickBreakerMut where

import qualified Data.Vector.Mutable as M
import qualified Data.Vector         as U
import Graphics.UI.SDL hiding (flip)

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Random

width, height, depth, blockW, blockH, secsPerFrame :: Int
width  = 640
height = 420
depth  = 32
blockW = width
blockH = height `quot` 3
secsPerFrame = 30 `quot` 1000

data Particle        = Particle !Int !Int !Int !Int !Pixel
data Paddle          = Paddle !Int !Int !Int
data GameState       = GS [Particle] Block
type Block           = U.Vector (Maybe Pixel)
type BlockST s       = M.STVector s (Maybe Pixel)

genBlock :: Block
genBlock = U.fromList [Just $ Pixel pix | pix <- [0,2800..]]

collisionPaddle :: Paddle -> Particle -> Bool
collisionPaddle (Paddle px pw ph) (Particle x y _ dy _) =
    y >= height - ph && x >= px && x <= pw + pw && dy > 0

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = liftM listToMaybe . filterM p

approach :: BlockST s -> Particle -> MaybeT (ST s) Int
approach st (Particle x y dx dy _) = MaybeT $ findM (liftM isJust . M.read st) path
    where enum 0 = repeat 0
          enum n = let i = signum n in enumFromThenTo i (i+i) n
          path   = [idx (x+dx') (y+dy') | (dx', dy') <- zip (enum dx) (enum dy) , y + dy' <= blockH, x + dx' <= blockW]
          idx i j = (i - 1) + max 0 (j - 1) * blockW

collisionBlock :: BlockST s -> Particle -> MaybeT (ST s) (Pixel, Int)
collisionBlock st pt@(Particle _ y _ dy _)
    | dy > 0 && y > blockH      = mzero
    | dy < 0 && y > blockH - dy = mzero
    | otherwise = approach st pt >>= \i -> MaybeT $ do
            pix <- liftM fromJust $ M.read st i
            M.write st i Nothing
            return $ Just (pix, i)

checkCollisions :: Paddle -> GameState -> GameState
checkCollisions pd (GS ps bs) = runST $ do
    bs' <- U.unsafeThaw bs
    liftM2 GS (join `fmap` mapM (go bs') ps) (U.unsafeFreeze bs')
    where go st pt@(Particle x y dx dy pix)
            | collisionPaddle pd pt = return [bar]
            | otherwise = maybe [pt] (\(p, i) -> [blk, randomParticle i p]) `fmap` runMaybeT (collisionBlock st pt)
            where bar = Particle x (min height y) dx (-dy) pix
                  blk = Particle x y dx (abs dy) pix

randomParticle :: Int -> Pixel -> Particle
randomParticle i = Particle x y (ceiling $ dx * 10) (ceiling $ dy * 9 + 1)
    where (x, y)  = (i `rem` blockW, i `quot` blockW)
          (dx, g) = randomR (-1.0, 1.0) (mkStdGen $ x + y) :: (Float, StdGen)
          (dy, _) = randomR (0.0, 1.0) g :: (Float, StdGen)

updateParticle :: Particle -> Particle
updateParticle (Particle x y dx dy pix) = Particle (x + dx') (y + dy') dx' dy' pix
    where dx' = if (x > width && dx > 0) || (x < 0 && dx < 0) then (-dx) else dx
          dy' = if y < 0 && dy < 0 then (-dy) else dy

updateGame :: Paddle -> GameState -> GameState
updateGame pd gs = GS (map updateParticle $ filter inBounds ps) bs
    where GS ps bs = checkCollisions pd gs
          inBounds (Particle _ y _ _ _) = height >= y

gameOver :: GameState -> Bool
gameOver (GS ps bs) = null ps || U.null bs
