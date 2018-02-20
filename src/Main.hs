{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}

module Main where


import Prelude hiding (map)

import Control.Monad          (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool              (bool)
import Data.Maybe             (fromMaybe)
import Data.Bifunctor         (bimap)
import Data.List              (delete, find)
import Data.Foldable          (traverse_)

import Linear                 (V2(V2))

import qualified Data.Vector as V (toList, (!?), ifoldr')
import qualified UI.NCurses  as C

import qualified MapGenerator as MG
import Input
import TileMap

-------------------------------------------------------------------------------

data Palette = Palette {
      black   ∷ C.ColorID
    , red     ∷ C.ColorID
    , green   ∷ C.ColorID
    , yellow  ∷ C.ColorID
    , blue    ∷ C.ColorID
    , magenta ∷ C.ColorID    
    , cyan    ∷ C.ColorID
    , white   ∷ C.ColorID
    }

-------------------------------------------------------------------------------

type Opened = Bool
type CursesDrawable = (Char, [C.Attribute])

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


tileFromCharacter ∷ Palette → Char → Tile CursesDrawable
tileFromCharacter p c@('╔') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╗') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╚') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╝') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('═') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('║') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╟') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╤') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╢') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('╧') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])

tileFromCharacter p c@('│') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('─') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┘') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('└') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┌') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┐') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('├') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┤') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┴') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('┬') = Wall (c, [ C.AttributeColor (yellow p), C.AttributeDim ])

tileFromCharacter p c@('+') = Door False (c, [ C.AttributeColor (yellow p), C.AttributeDim ])
tileFromCharacter p c@('/') = Door True  (c, [ C.AttributeColor (yellow p), C.AttributeDim ])

tileFromCharacter p c@(',')  = Grass (c, [ C.AttributeColor (green p) ])
tileFromCharacter p c@('\'') = Grass (c, [ C.AttributeColor (green p) ])
tileFromCharacter p c@('"')  = Grass (c, [ C.AttributeColor (green p) ])

tileFromCharacter p c@('.')  = Rock (c, [ C.AttributeColor (white p), C.AttributeDim ])

tileFromCharacter p _ = Wall ('?', [ C.AttributeColor (red p), C.AttributeBold ]) -- TODO crash

-------------------------------------------------------------------------------

data Character a = Character {
      charName  ∷ String
    , inventory ∷ [a]
    }
    deriving (Show)


addToInventory ∷ a → Character a → Character a
addToInventory i c = c { inventory = inventory c ++ [i] }


removeFromInventory ∷ (Eq a) ⇒ a → Character a → Character a
removeFromInventory i c = c { inventory = delete i (inventory c) }

-------------------------------------------------------------------------------

data Entity a = Entity {
      position ∷ V2 Int   -- <-- Do we want negative positions?
    , object   ∷ a
    }
    deriving (Eq, Show, Functor)

--------------------------------------------------------------------------------

data Game a b c d = Game {
      running ∷ Bool

    , avatar  ∷ Entity a
    , items   ∷ [Entity b]
    , enemies ∷ [Entity c]

    , map     ∷ TileMap d

    , debugStatus ∷ String
    }


update ∷ Event → Game (Character Char) Char c (Tile d) → Game (Character Char) Char c (Tile d)
update (Move d)   og = (\g → setStatus (show . avatar $ g) g) $ moveAvatar d og
update Attack     og = og
update Open       og = og
update Close      og = og
update Get        og = (\g → setStatus (show . avatar $ g) g) $ pickUpItem (position (avatar og)) 'X' og
update Talk       og = og
update Idle       og = og
update Quit       og = og { running     = False }


setStatus ∷ String → Game a b c d → Game a b c d
setStatus s g = g { debugStatus = s }


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


pickUpItem ∷ (Eq b) ⇒ V2 Int → b → Game (Character b) b c (Tile d) → Game (Character b) b c (Tile d)
pickUpItem v rv og = fromMaybe og $ do
    obj ← find ((==v) . position) (items og)
    pure $ og { avatar = (avatar og) { object = addToInventory (object obj) (object (avatar og)) }
              , items  = replaceObject obj rv (items og)
              }
    where
        replaceObject ∷ (Eq a) ⇒ Entity a → a → [Entity a] → [Entity a]
        replaceObject x rv = foldr (\lx nl → if x == lx
                                               then lx { object = rv } : nl
                                               else lx : nl) []


directionToVector ∷ Direction → V2 Int
directionToVector North     = V2  0 -1
directionToVector South     = V2  0  1
directionToVector East      = V2  1  0
directionToVector West      = V2 -1  0
directionToVector NorthWest = V2 -1 -1
directionToVector NorthEast = V2  1 -1
directionToVector SouthWest = V2 -1  1
directionToVector SouthEast = V2  1  1

--------------------------------------------------------------------------------

render ∷ Game a Char c (Tile CursesDrawable) → C.Curses ()
render g = do
    w ← C.defaultWindow
    C.updateWindow w renderAction
    C.render
    where
        renderAction = do
            drawMap (map g)
            drawItems (items g)
            drawCharacter (position . avatar $ g)
            drawDebugStatus (debugStatus g)

{-
ColorBlack

ColorRed     
ColorGreen   
ColorYellow  
ColorBlue    
ColorMagenta     
ColorCyan    
ColorWhite
-}

{-
AttributeColor ColorID  
AttributeBold   
AttributeDim    

AttributeStandout   
AttributeUnderline  
AttributeReverse    
AttributeBlink  
AttributeAltCharset 
AttributeInvisible  
AttributeProtect    
AttributeHorizontal 
AttributeLeft   
AttributeLow    
AttributeRight  
AttributeTop    
AttributeVertical   
-}

drawMap ∷ TileMap (Tile CursesDrawable) → C.Update ()
drawMap m = mapM_ drawTile . zip [0..] . V.toList . mapData $ m
    where
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord i (width m)
            C.moveCursor (fromIntegral y) (fromIntegral x) 
            C.drawGlyph (uncurry C.Glyph (drawableFromTile t))


drawItems ∷ [Entity Char] → C.Update ()
drawItems = traverse_ drawItem 
    where
        drawItem (Entity (V2 x y) ch) = do
            C.moveCursor (fromIntegral y) (fromIntegral x) 
            C.drawGlyph (C.Glyph ch [ C.AttributeBold ])
            


drawCharacter ∷ V2 Int → C.Update ()
drawCharacter (V2 x y) =
    let pcGlyph = C.Glyph '@' [ C.AttributeBold ]
    in  do
        C.moveCursor (fromIntegral y) (fromIntegral x) 
        C.drawGlyph pcGlyph


drawDebugStatus ∷ String → C.Update ()
drawDebugStatus s = do
    C.moveCursor 1 1
    C.drawString s

--------------------------------------------------------------------------------

main ∷ IO ()
main = C.runCurses $ do
    void $ C.setCursorMode C.CursorInvisible
    C.setEcho False
    (r, c) ← bimap fromIntegral (fromIntegral . (1`subtract`)) <$> C.screenSize
    p ← preparePalette
    m ← liftIO $ MG.generate c r (tileFromCharacter p)

    gameLoop (initialGame m)
    where
        initialChar   = Entity (V2 0 0) $ Character "John" []
        initialGame m = Game True initialChar (createItemsFromMap m) [] m ""
        gameLoop g = do
            e ← nextEvent
            let ng = update e g
            render ng
            when (running ng) $
                gameLoop ng
        preparePalette = pure Palette
                         <*> C.newColorID C.ColorBlack   C.ColorBlack 1
                         <*> C.newColorID C.ColorRed     C.ColorBlack 2
                         <*> C.newColorID C.ColorGreen   C.ColorBlack 3
                         <*> C.newColorID C.ColorYellow  C.ColorBlack 4
                         <*> C.newColorID C.ColorBlue    C.ColorBlack 5
                         <*> C.newColorID C.ColorMagenta C.ColorBlack 6
                         <*> C.newColorID C.ColorCyan    C.ColorBlack 7
                         <*> C.newColorID C.ColorWhite   C.ColorBlack 8

        createItemsFromMap ∷ TileMap (Tile CursesDrawable) → [Entity Char]
        createItemsFromMap m = 
            let getRocks ix t l =
                 if fst (drawableFromTile t) == '.'
                   then Entity (fromIntegral <$> linear2Coord (fromIntegral ix) (width m)) '.' : l
                   else l
            in  V.ifoldr' getRocks [] . mapData $ m

