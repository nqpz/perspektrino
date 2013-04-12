{-# LANGUAGE TupleSections #-}

module Main where

import GHC.Int
import Prelude hiding (Left, Right)
import Data.List
import Data.Word
import Data.Bits
import Data.Ratio
import Control.Monad
import Control.Concurrent (threadDelay)
import qualified System.Random as R
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import Graphics.UI.SDL.Keysym


type Position3D = (Int, Int, Int)
type World = [Position3D]
type Position2D = (Ratio Int, Ratio Int)
type Polygon = [Position2D]
data Axis = X | Y | Z

data Direction1D = Clock | CClock
                 deriving (Show, Read, Eq, Ord)
data Direction2D = Up | Right | Down | Left
                 deriving (Show, Read, Eq, Ord)
instance Enum Direction2D where
  succ Up    = Right
  succ Right = Down
  succ Down  = Left
  succ Left  = Up

  toEnum 0 = Up
  toEnum 1 = Right
  toEnum 2 = Down
  toEnum 3 = Left
  toEnum n = toEnum $ n `mod` 4
  
  fromEnum Up    = 0
  fromEnum Right = 1
  fromEnum Down  = 2
  fromEnum Left  = 3


worldToPoly :: World -> [Polygon]
worldToPoly = concatMap cubePoly
              . filter posInView
              . sortBy drawFirstCompare
  where drawFirstCompare (x0, y0, z0) (x1, y1, z1)
          = compare (z1, abs x1, abs y1) (z0, abs x0, abs y0)

        posInView (x, y, z) = z >= 0
                              && y >= start - 1 && y <= end
                              && x >= start - 1 && x <= end
          where (start, end) = (-z, z + 1)

cubePoly :: Position3D -> [Polygon]
cubePoly (x, y, z)
  = polys x Left ++ polys y Up
    ++ [squarePoly (top z x) (top z y) (size z) (size z)]
  where size :: Int -> Ratio Int
        size z = 1 % (z * 2 + 1)

        top :: Int -> Int -> Ratio Int
        top z rel = fromIntegral (z + rel) * size z

        bottom :: Int -> Int -> Ratio Int
        bottom z rel = top z rel + size z

        fitsTop :: Int -> Bool
        fitsTop rel = top z rel > top (z + 1) rel

        fitsBottom :: Int -> Bool
        fitsBottom rel = bottom z rel < bottom (z + 1) rel

        polys :: Int -> Direction2D -> [Polygon]
        polys rel d | fitsTop rel == fitsBottom rel = []
                    | fitsTop rel = [trapezoidShape d]
                    | fitsBottom rel = [trapezoidShape $ succ $ succ d]

        trapezoidShape :: Direction2D -> Polygon
        trapezoidShape dir = shape' $ case dir of
          Up -> (top, top, bottom, top)
          Right -> (bottom, top, bottom, bottom)
          Down -> (top, bottom, bottom, bottom)
          Left -> (top, top, top, bottom)
          where shape' (a, b, c, d) = [ (a z x, b z y)
                                      , (a (z + 1) x, b (z + 1) y)
                                      , (c (z + 1) x, d (z + 1) y)
                                      , (c z x, d z y)
                                      ]

squarePoly :: Ratio Int -> Ratio Int -> Ratio Int -> Ratio Int -> Polygon
squarePoly x y w h = [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

turn :: Direction2D -> World -> World
turn dir = map $ \(x, y, z) ->
  let z' = z + 1
      (x', y', z'') = case dir of
        Right -> (-z', y, x)
        Left  -> (z', y, -x)
        Down  -> (x, -z', y)
        Up    -> (x, z', -y)
  in (x', y', z'' - 1)

move :: Direction2D -> World -> World
move dir = map $ \(x, y, z) -> case dir of
  Right -> (x - 1, y, z)
  Left  -> (x + 1, y, z)
  Down  -> (x, y, z + 1)
  Up    -> (x, y, z - 1)

width :: Int
width  = 1920
height :: Int
height = 1080

explore :: World -> IO ()
explore world = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Perspektrino Explorer" "perspektrino-explore"
  screenSurf <- SDL.getVideoSurface
  explore' screenSurf world
  where explore' :: SDL.Surface -> World -> IO ()
        explore' screenSurf world = do
          renderWorldToScreen screenSurf world
          readEvent screenSurf world
        renderWorldToScreen screenSurf world = do
          SDL.fillRect screenSurf Nothing (SDL.Pixel 0x00000000)
          mapM_ (drawPoly screenSurf) (worldToPoly world)
          SDL.flip screenSurf
        drawPoly :: SDL.Surface -> Polygon -> IO Bool
        drawPoly screenSurf points = do
          pixel <- R.randomRIO (0, 2^24 - 1)
          let pixel' = 0x000000ff + pixel `shiftL` 8
          SDLp.filledPolygon screenSurf (map point16 points) (SDL.Pixel pixel')
          where point16 (x, y) = (fromIntegral (round (x * fromIntegral width)),
                                  fromIntegral (round (y * fromIntegral height)))
        readEvent screenSurf world = do
          event <- SDL.pollEvent
          case eventAction event of
            Nothing -> SDL.quit
            Just Nothing -> threadDelay 1000 >> readEvent screenSurf world
            Just (Just action) -> explore' screenSurf $ action world

        eventAction :: SDL.Event -> Maybe (Maybe (World -> World))
        eventAction (SDL.KeyDown (Keysym k mods _))
          | k == SDLK_UP && ctrl = j $ turn Down
          | k == SDLK_UP = j $ move Up
          | k == SDLK_DOWN && ctrl = j $ turn Up
          | k == SDLK_DOWN = j $ move Down
          | k == SDLK_RIGHT && ctrl = j $ move Right
          | k == SDLK_RIGHT = j $ turn Right
          | k == SDLK_LEFT && ctrl = j $ move Left
          | k == SDLK_LEFT = j $ turn Left
          where ctrl = any (`elem` mods) [KeyModCtrl, KeyModLeftCtrl,
                                          KeyModRightCtrl]
                j = Just . Just
        eventAction SDL.Quit = Nothing
        eventAction _ = Just Nothing


randomWorld :: Int -> Int -> Int -> IO World
randomWorld nblocks start end = replicateM nblocks $ do
  [x, y, z] <- replicateM 3 $ R.randomRIO (start, end)
  return (x, y, z)

dikuKantine :: World
dikuKantine = surf (-1) ++ surf 3 ++ wall (-4) ++ wall 4 ++ endWall 48
              ++ blocks (-4) (-1) (-1) 3 10 10
              ++ blocks (-1) (-1) (-1) 1 12 30
  where surf y = [ (x, y, z) | x <- [-4..4], z <- [0..48] ]
        wall x = [ (x, y, z) | y <- [-1..3], z <- [0..48] ]
        endWall z = [ (x, y, z) | y <- [-1..3], x <- [-4..4] ]
        blocks x0 x1 y0 y1 z0 z1 = [ (x, y, z) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1] ]
  
box :: World
box = concat [ [ (x, y, z) | x <- [-5..5], y <- [-5..5], z <- [-5,5] ]
             , [ (x, y, z) | x <- [-5..5], z <- [-5..5], y <- [-5,5] ]
             , [ (x, y, z) | y <- [-5..5], z <- [-5..5], x <- [-5,5] ]
             , [ (2, 2, 2), (3, -5, 1) ]
             ]

translate :: Position3D -> World -> World
translate (x0, y0, z0) = map (\(x, y, z) -> (x + x0, y + y0, z + z0))

rotate :: Axis -> Direction1D -> World -> World
rotate = undefined

room :: Int -> Int -> Int -> World
room x0 y0 z0 = concat [ [ (x, y, z) | x <- [0..xw], y <- [0..yw], z <- [0, zw] ]
                       , [ (x, y, z) | z <- [0..zw], x <- [0..xw], y <- [0, yw] ]
                       , [ (x, y, z) | y <- [0..yw], z <- [0..zw], x <- [0, xw] ]
                       ]
  where (xw, yw, zw) = (x0 + 1, y0 + 1, z0 + 1)

hole :: Position3D -> World -> World
hole p = filter (/= p)

alphaBase :: World
alphaBase = hole (0, 0, 14)
            $ hole (0, 0, 5)
            $ hole (0, -1, 5)
            $ translate (0, -3, -1) solids
  where startRoom = translate (-3, 0, 0) $ room 5 3 5
        corridor0 = translate (-1, 1, 6) $ room 1 2 8
        corridor1 = translate (-1, 2, 15) $ room 8 1 1
        endRoom = translate (8, 2, 10) $ room 10 10 10
        solids = startRoom ++ corridor0 ++ corridor1 ++ endRoom