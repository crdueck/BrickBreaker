module BrickBreaker where

import qualified Data.Map as M
import Graphics.UI.SDL hiding (flip)

import Control.Arrow (first)
import Data.List     (find)
import Data.Maybe    (fromJust)
import System.Random (randomR, mkStdGen, StdGen)

width, height, depth, fps, blockW, blockH, secsPerFrame :: Int
width  = 640
height = 420
depth  = 32
fps    = 40

blockW = width
blockH = height `quot` 3
secsPerFrame = 1000 `quot` fps

data Particle        = Particle { partX, partY, partDX, partDY :: {-# UNPACK #-} !Int, pixel :: {-# UNPACK #-} !Pixel }
data Paddle          = Paddle {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
data GameState       = GS [Particle] Block
type Position        = (Int, Int)
type Block           = M.Map Position Pixel

genBlock :: Block
<<<<<<< HEAD
genBlock = M.fromAscList $ zip [(w, h) | w <- [0..blockW], h <- [0..blockH]] [Pixel pix | pix <- [0,2800..]]
=======
genBlock = M.fromAscList $ zip pos pixels
    where pos    = [(w, h) | w <- [0..blockW], h <- [0..blockH]]
          pixels = [Pixel pix | pix <- [0,2800..]]
>>>>>>> c6818df20ca814b5521394998d28b3a7cbc5934f

approach :: Particle -> Block -> Maybe Position
approach (Particle x y dx dy _) bs = find (`M.member` bs) path
    where enum 0 = repeat 0
          enum n = let i = signum n in enumFromThenTo i (i+i) n
          path   = [(x + dx', y + dy') | (dx', dy') <- zip (enum dx) (enum dy)]

collisionBlock :: Particle -> Block -> Maybe (Pixel, Block)
collisionBlock pt@(Particle _ y _ dy _) bs
    | dy > 0 && y > blockH      = Nothing
    | dy < 0 && y > blockH - dy = Nothing
    | otherwise = (first fromJust . searchRemove) `fmap` approach pt bs
    where searchRemove = flip (M.updateLookupWithKey (\_ _ -> Nothing)) bs

collisionPaddle :: Paddle -> Particle -> Bool
collisionPaddle (Paddle px pw ph) (Particle x y _ dy _) =
    y >= height - ph && x >= px && x <= px + pw && dy > 0

checkCollisions :: Paddle -> GameState -> GameState
checkCollisions pd (GS ps bs) = foldr go (GS [] bs) ps
    where go pt@(Particle x y dx dy pix) (GS ps bs)
            | collisionPaddle pd pt = GS (bar:ps) bs
<<<<<<< HEAD
            | otherwise = case collisionBlock pt bs of
                     Just (pix', bs') -> GS (blk:randomParticle (x,y) pix':ps) bs'
=======
            | otherwise =
                case collisionBlock pt bs of
                     Just (pix', bs') -> GS (blk:randomParticle pt pix':ps) bs'
>>>>>>> c6818df20ca814b5521394998d28b3a7cbc5934f
                     Nothing          -> GS (pt:ps) bs
            where bar = Particle x (min y height) dx (-dy) pix
                  blk = Particle x y dx (abs dy) pix

randomParticle :: Position -> Pixel -> Particle
randomParticle (x, y) = Particle x y (ceiling $ dx * 10) (ceiling $ dy * 9 + 1)
    where (dx, g) = randomR (-1.0, 1.0) (mkStdGen $ x + y) :: (Float, StdGen)
          (dy, _) = randomR (0.1, 1.0) g :: (Float, StdGen)

updateParticle :: Particle -> Particle
updateParticle (Particle x y dx dy pix) = Particle (x + dx') (y + dy') dx' dy' pix
    where dx' = if (x > width && dx > 0) || (x < 0 && dx < 0) then (-dx) else dx
          dy' = if y < 0 && dy < 0 then (-dy) else dy

updateGame :: Paddle -> GameState -> GameState
updateGame pd gs = GS (map updateParticle $ filter inBounds ps) bs
    where GS ps bs = checkCollisions pd gs
          inBounds = (<= height) . partY

gameOver :: GameState -> Bool
gameOver (GS ps bs) = null ps || M.null bs
