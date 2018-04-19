{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where


import Prelude hiding (map)

import Control.Lens           (makeLenses, (<>~), (%~), (.~), (^.), view, views,
                               set)
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
      _charName  ∷ String
    , _inventory ∷ [a]
    }
    deriving (Show)
makeLenses ''Character


addToInventory ∷ a → Character a → Character a
addToInventory i = inventory <>~ [i]


removeFromInventory ∷ (Eq a) ⇒ a → Character a → Character a
removeFromInventory i = inventory %~ delete i

-------------------------------------------------------------------------------

data Entity a = Entity {
      _position ∷ V2 Int   -- <-- Do we want negative positions?
    , _object   ∷ a
    }
    deriving (Eq, Show, Functor)
makeLenses ''Entity

--------------------------------------------------------------------------------

data Game a b c d = Game {
      _running ∷ Bool

    , _avatar  ∷ Entity a
    , _items   ∷ [Entity b]
    , _enemies ∷ [Entity c]

    , _map     ∷ TileMap d

    , _debugStatus ∷ String
    }
makeLenses ''Game


update ∷ Event → Game (Character Char) Char c (Tile d) → Game (Character Char) Char c (Tile d)
update (Move d)   og = displayCharacterStatus $ moveAvatar d og
update Attack     og = og
update Open       og = og
update Close      og = og
update Get        og = displayCharacterStatus $ pickUpItem (og ^. avatar.position) 'X' og
update Talk       og = og
update Idle       og = og
update Quit       og = running .~ False $ og


displayCharacterStatus ∷ (Show a) ⇒ Game a b c d → Game a b c d
displayCharacterStatus g = debugStatus .~ (views avatar show g) $ g


-- Do change only the entity, not the game, like this:
-- moveAvatar ∷ Direction → TileMap (Tile a) → Entity a → Entity a
moveAvatar ∷ Direction → Game a b c (Tile d) → Game a b c (Tile d)
moveAvatar d og = let op  = og ^. avatar.position
                      np  = fmap (max 0) $ op + directionToVector d
                      nap = fromMaybe op $ do
                                mapTile ← (og ^. map.mapData) V.!? fromIntegral (coord2Linear np (og ^. map.width))
                                case mapTile of
                                    (Wall  _)   → pure op
                                    (Door  t _) → pure $ bool op np t
                                    _           → pure np
                  in  avatar .~ Entity nap (og ^. avatar.object) $ og


pickUpItem ∷ (Eq b) ⇒ V2 Int → b → Game (Character b) b c (Tile d) → Game (Character b) b c (Tile d)
pickUpItem v rv og = fromMaybe og $ do
    obj ← find ((==v) . view position) (og ^. items)
    pure $ 
        avatar.object %~ addToInventory (obj ^. object) $
        items         %~ replaceObject obj rv $
        og
    where
        replaceObject ∷ (Eq a) ⇒ Entity a → a → [Entity a] → [Entity a]
        replaceObject x rv = foldr (\lx nl → if x == lx
                                               then (object .~ rv $ lx) : nl
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
            drawMap (g ^. map)
            drawItems (g ^. items)
            drawCharacter (g ^. avatar.position)
            drawDebugStatus (g ^. debugStatus)

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
drawMap m = mapM_ drawTile . zip [0..] . V.toList . view mapData $ m
    where
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord i (m ^. width)
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
            when (ng ^. running) $
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
                   then Entity (fromIntegral <$> linear2Coord (fromIntegral ix) (m ^. width)) '.' : l
                   else l
            in  V.ifoldr' getRocks [] . view mapData $ m

