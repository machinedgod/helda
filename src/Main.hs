{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}

module Main where


import Prelude hiding (map)

import Control.Monad.IO.Class (liftIO)
import Control.Monad          (when)

import Linear                 (V2(V2))

import qualified Data.Vector as V (zip, toList)
import qualified UI.NCurses  as C

import Input
import TileMap

-------------------------------------------------------------------------------

main ∷ IO ()
main = C.runCurses $ do
    C.setCursorMode C.CursorInvisible
    C.setEcho       False
    m ← liftIO $ loadFromFile "res/test_map.tmap" id
    gameLoop (initialGame m)
    where
        initialGame m = Game True (Entity (V2 0 0) Character) [] [] m
        gameLoop g = do
            e ← nextEvent
            let ng = update e g
            render ng
            when (running ng) $
                gameLoop ng

--------------------------------------------------------------------------------

render ∷ Game a b c Char → C.Curses ()
render g = C.defaultWindow >>= (`C.updateWindow` renderAction) >> C.render
    where
        renderAction = do
            C.clear
            drawMap (map g)
            drawCharacter (position . avatar $ g)


drawMap ∷ TileMap Char → C.Update ()
drawMap map = mapM_ drawTile . zip [0..] . V.toList . mapData $ map
    where
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord i (width map)
            C.moveCursor (fromIntegral y) (fromIntegral x) 
            C.drawGlyph (C.Glyph t [])


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

data Character = Character deriving(Show)
data Item      = Item      deriving(Show)


data Game a b c d = Game {
      running ∷ Bool

    , avatar  ∷ Entity a
    , items   ∷ [Entity b]
    , enemies ∷ [Entity c]

    , map     ∷ TileMap d
    }


update ∷ Event → Game Character Item Character d → Game Character Item Character d
update (Move d) og = moveAvatar d og
update Attack   og = og
update Open     og = og
update Close    og = og
update Get      og = og
update Talk     og = og
update Idle     og = og

update Quit     og = og { running = False }


moveAvatar ∷ Direction → Game Character a b c → Game Character a b c
moveAvatar d og = let olda = avatar og
                      np   = fmap (max 0) $ position olda + directionToVector d
                  in  og { avatar = Entity np (object olda) }


directionToVector ∷ Direction → V2 Int
directionToVector North     = V2  0 -1
directionToVector South     = V2  0  1
directionToVector East      = V2  1  0
directionToVector West      = V2 -1  0
directionToVector NorthWest = V2 -1 -1
directionToVector NorthEast = V2  1 -1
directionToVector SouthWest = V2 -1  1
directionToVector SouthEast = V2  1  1


