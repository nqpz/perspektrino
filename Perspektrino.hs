{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (Left, Right)
import Data.List
import Data.Function (on)
import Data.Word
import Data.Ratio
import Graphics.Rendering.Cairo
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Keysym
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified System.Random as R

type Position2D = (Rational, Rational)
type Position3D = (Int, Int, Int)
data Direction = Up | Right | Down | Left
                deriving (Show, Read, Eq, Ord)
instance Enum Direction where
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
                         
type World = [Position3D]
type Color = (Double, Double, Double, Double)
data Drawable = Polygon [Position2D]
              deriving (Show, Read, Eq, Ord)
data DrawAttribute = Fill Color
                   | Gradient Color Color Direction
                   deriving (Show, Read, Eq, Ord)
type Drawing = (Drawable, DrawAttribute)
type PolygonRender = [Drawing]


worldToPoly :: World -> PolygonRender
worldToPoly = concatMap blockToPoly
              . filter posInView
              . sortBy drawFirstCompare
  where drawFirstCompare (x0, y0, z0) (x1, y1, z1)
          = compare (z1, abs x1, abs y1) (z0, abs x0, abs y0)
        posInView (x, y, z) = z >= 0
                              && y >= start - 1 && y <= end
                              && x >= start - 1 && x <= end
          where (start, end) = (-z, z + 1)

blockToPoly :: Position3D -> PolygonRender
blockToPoly (x, y, z)
  = polys x Left ++ polys y Up
    ++ [filledSquare (top z x) (top z y) (size z) (size z)]
  where size z = 1 % (fromIntegral z * 2 + 1)
        top z rel = fromIntegral (z + rel) * size z
        bottom z rel = top z rel + size z
        fitsTop rel = top z rel > top (z + 1) rel
        fitsBottom rel = bottom z rel < bottom (z + 1) rel
        polys rel d | fitsTop rel == fitsBottom rel = []
                    | fitsTop rel = [trapezoidShape d]
                    | fitsBottom rel = [trapezoidShape $ succ $ succ d]
        trapezoidShape dir = gradientTrapezoid dir . shape' $ case dir of
          Up -> (top, top, bottom, top)
          Right -> (bottom, top, bottom, bottom)
          Down -> (top, bottom, bottom, bottom)
          Left -> (top, top, top, bottom)
          where shape' (a, b, c, d) = [ (a z x, b z y)
                                      , (a (z + 1) x, b (z + 1) y)
                                      , (c (z + 1) x, d (z + 1) y)
                                      , (c z x, d z y)
                                      ]

filledSquare :: Rational -> Rational -> Rational -> Rational -> Drawing
filledSquare x y w h = (Polygon [(x, y), (x + w, y),
                                 (x + w, y + h), (x, y + h)],
                        Fill (0.2, 0.2, 0.2, 1.0))

gradientTrapezoid :: Direction -> [Position2D] -> Drawing
gradientTrapezoid ldir points
  = (Polygon points, Gradient (0.2, 0.2, 0.2, 1.0) (0.3, 0.3, 0.3, 1.0) ldir)

turn :: Direction -> World -> World
turn dir = map $ \(x, y, z) ->
  let z' = z + 1
      (x', y', z'') = case dir of
        Right -> (-z', y, x)
        Left  -> (z', y, -x)
        Down  -> (x, -z', y)
        Up    -> (x, z', -y)
  in (x', y', z'' - 1)

move :: Direction -> World -> World
move dir = map $ \(x, y, z) -> case dir of
  Right -> (x - 1, y, z)
  Left  -> (x + 1, y, z)
  Down  -> (x, y, z + 1)
  Up    -> (x, y, z - 1)

width  = 800
height = 800

renderToPNG :: FilePath -> PolygonRender -> Int -> Int -> IO ()
renderToPNG fname drawings width height = do
  surf <- createImageSurface FormatARGB32 width height
  renderWith surf (draw drawings)
  surfaceWriteToPNG surf fname
  surfaceFinish surf
  where draw :: PolygonRender -> Render ()
        draw drawings = do
          setFillRule FillRuleWinding
          setSourceRGBA 1.0 1.0 1.0 1.0
          rectangle 0 0 (fromIntegral width) (fromIntegral height)
          fill
          mapM_ drawDrawing drawings
        drawDrawing (Polygon points, attr) = do
          setSourceRGBA 0.0 0.0 0.0 1.0
          moveTo x y
          mapM_ (uncurry lineTo) points'
          closePath
          strokePreserve
          setAttr attr
          fill
          where ((x, y) : points')
                  = map (\(x, y) -> (fromRational (fromIntegral width * x),
                                     fromRational (fromIntegral height * y)))
                    points
                setAttr (Fill (r, g, b, a)) = setSourceRGBA r g b a
                setAttr (Gradient (r0, g0, b0, a0) (r1, g1, b1, a1) ldir)
                  = setSourceRGBA r0 g0 b0 a0 -- FIXME

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
          renderToPNG "temp.png" (worldToPoly world) width height
          surf <- SDLi.load "temp.png"
          SDL.blitSurface surf Nothing screenSurf Nothing
          SDL.flip screenSurf
        readEvent screenSurf world = do
          event <- SDL.pollEvent
          case eventAction event of
            Nothing -> SDL.quit
            Just Nothing -> threadDelay 100000 >> readEvent screenSurf world
            Just (Just action) -> explore' screenSurf $ action world

        eventAction :: SDL.Event -> Maybe (Maybe (World -> World))
        eventAction (SDL.KeyDown (Keysym k mods _))
          | k == SDLK_UP    = w Up
          | k == SDLK_RIGHT = w Right
          | k == SDLK_DOWN  = w Down
          | k == SDLK_LEFT  = w Left
          where w = Just . Just . w'
                w' | any (`elem` mods) [KeyModCtrl, KeyModLeftCtrl,
                                        KeyModRightCtrl] = turn
                   | otherwise = move
        eventAction SDL.Quit = Nothing
        eventAction _ = Just Nothing

generateWorld :: Int -> Int -> Int -> IO [Position3D]
generateWorld nblocks start end = replicateM nblocks $ do
  [x, y, z] <- replicateM 3 $ R.randomRIO (start, end)
  return (x, y, z)

exploreGeneratedWorld :: Int -> Int -> Int -> IO ()
exploreGeneratedWorld nblocks start end = generateWorld nblocks start end >>= explore


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

saveFilm :: World -> [World -> World] -> IO ()
saveFilm world actions = mapM_ render $ zip [0..] scenes
  where scenes = scanl (flip ($)) world actions
        render (i, world') = do
          let fn = (take (8 - length (show i))
                    (repeat '0') ++ show i ++ ".png")
          renderToPNG fn (worldToPoly world') width height
          putStrLn fn

save_film0 = do
  w <- generateWorld 10000 (-1000) 1000
  saveFilm w $ take 1000 $ repeat (move Up)

main = save_film0