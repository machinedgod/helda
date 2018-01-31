{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}

module Main where


import Prelude hiding (map)

import Control.Monad          (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool              (bool)
import Data.Maybe             (fromMaybe)

import Linear                 (V2(V2))

import qualified Data.Vector as V (toList, (!?))
import qualified UI.NCurses  as C

import qualified MapGenerator as MG
import Input
import TileMap

-------------------------------------------------------------------------------

type Opened = Bool

data Tile a = Floor a
            | Wall  a
            | Door  Opened a

            | Grass a
            | Rock  a
            deriving (Show)


drawableFromTile ∷ Tile a → a
drawableFromTile (Floor  x) = x
drawableFromTile (Wall   x) = x
drawableFromTile (Door _ x) = x
drawableFromTile (Grass  x) = x
drawableFromTile (Rock   x) = x


tileFromCharacter ∷ Char → Tile Char
tileFromCharacter c@('╔') = Wall c
tileFromCharacter c@('╗') = Wall c
tileFromCharacter c@('╚') = Wall c
tileFromCharacter c@('╝') = Wall c
tileFromCharacter c@('═') = Wall c
tileFromCharacter c@('║') = Wall c
tileFromCharacter c@('╟') = Wall c
tileFromCharacter c@('╤') = Wall c
tileFromCharacter c@('╢') = Wall c
tileFromCharacter c@('╧') = Wall c

tileFromCharacter c@('│') = Wall c
tileFromCharacter c@('─') = Wall c
tileFromCharacter c@('┘') = Wall c
tileFromCharacter c@('└') = Wall c
tileFromCharacter c@('┌') = Wall c
tileFromCharacter c@('┐') = Wall c
tileFromCharacter c@('├') = Wall c
tileFromCharacter c@('┤') = Wall c
tileFromCharacter c@('┴') = Wall c
tileFromCharacter c@('┬') = Wall c


tileFromCharacter c@('+') = Door False c
tileFromCharacter c@('/') = Door True  c

tileFromCharacter c@(',')  = Grass c
tileFromCharacter c@('\'') = Grass c
tileFromCharacter c@('"')  = Grass c

tileFromCharacter c@('.')  = Rock c

tileFromCharacter _ = Wall '?'

-------------------------------------------------------------------------------

main ∷ IO ()
main = C.runCurses $ do
    void $ C.setCursorMode C.CursorInvisible
    C.setEcho       False
    --(w, h) ← bimap (fromIntegral . (1`subtract`)) (fromIntegral . (1`subtract`)) <$> C.screenSize
    m ← liftIO $ MG.generate 30 30 tileFromCharacter
    --m ← liftIO $ loadFromFile "res/test_map.tmap" tileFromCharacter
    gameLoop (initialGame m)
    where
        initialGame m = Game True (Entity (V2 0 0) "John") [] [] m
        gameLoop g = do
            e ← nextEvent
            let ng = update e g
            render ng
            when (running ng) $
                gameLoop ng

--------------------------------------------------------------------------------

render ∷ Game a b c (Tile Char) → C.Curses ()
render g = C.defaultWindow >>= (`C.updateWindow` renderAction) >> C.render
    where
        renderAction = do
            C.clear
            drawMap (map g)
            drawCharacter (position . avatar $ g)


drawMap ∷ TileMap (Tile Char) → C.Update ()
drawMap m = mapM_ drawTile . zip [0..] . V.toList . mapData $ m
    where
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord i (width m)
            C.moveCursor (fromIntegral y) (fromIntegral x) 
            C.drawGlyph (C.Glyph (drawableFromTile t) [])


drawCharacter ∷ V2 Int → C.Update ()
drawCharacter (V2 x y) =
    let pcGlyph = C.Glyph '@' [ C.AttributeBold ]
    in  do
        C.moveCursor (fromIntegral y) (fromIntegral x) 
        C.drawGlyph pcGlyph

    
--------------------------------------------------------------------------------

data Entity a = Entity {
      position ∷ V2 Int   -- <-- Do we want negative positions?
    , object   ∷ a
    }
    deriving (Show, Functor)

--------------------------------------------------------------------------------

data Game a b c d = Game {
      running ∷ Bool

    , avatar  ∷ Entity a
    , items   ∷ [Entity b]
    , enemies ∷ [Entity c]

    , map     ∷ TileMap d
    }


update ∷ Event → Game a b c (Tile d) → Game a b c (Tile d)
update (Move d) og = moveAvatar d og
update Attack   og = og
update Open     og = og
update Close    og = og
update Get      og = og
update Talk     og = og
update Idle     og = og
update Quit     og = og { running = False }


-- Do change only the entity, not the game, like this:
-- moveAvatar ∷ Direction → TileMap (Tile a) → Entity a → Entity a
moveAvatar ∷ Direction → Game a b c (Tile d) → Game a b c (Tile d)
moveAvatar d og = let op  = position . avatar $ og
                      np  = fmap (max 0) $ op + directionToVector d
                      nap = fromMaybe op $ do
                                mapTile ← (mapData . map $ og) V.!? fromIntegral (coord2Linear np (width . map $ og))
                                case mapTile of
                                    (Wall  _)   → pure op
                                    (Door  t _) → pure $ bool op np t
                                    _           → pure np
                  in  og { avatar = Entity nap (object . avatar $ og) }


directionToVector ∷ Direction → V2 Int
directionToVector North     = V2  0 -1
directionToVector South     = V2  0  1
directionToVector East      = V2  1  0
directionToVector West      = V2 -1  0
directionToVector NorthWest = V2 -1 -1
directionToVector NorthEast = V2  1 -1
directionToVector SouthWest = V2 -1  1
directionToVector SouthEast = V2  1  1


