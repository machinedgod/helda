{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where


import Prelude hiding (map)

import Control.Lens           (makeLenses, (<>~), (%~), (.~), (^.), (%=),
                               view, views)
import Control.Monad          (when, void)
import Control.Monad.State    (execState)
import Control.Monad.IO.Class (liftIO)
import Data.Bool              (bool)
import Data.Maybe             (fromMaybe)
import Data.Bifunctor         (bimap)
import Data.List              (delete, find)
import Data.List.NonEmpty     (NonEmpty((:|)))
import Data.Foldable          (traverse_)

import Linear                 (V2(V2))

import qualified Data.Vector as V (toList, (!?), ifoldr', (//))
import qualified UI.NCurses  as C

import qualified MapGenerator as MG
import PDF
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

grassTiles ∷ [Char]
grassTiles = [ ',', '"', '\'' ]


rockTiles ∷ [Char]
rockTiles = [ '.' ]


flowerTiles ∷ [Char]
flowerTiles = [ 'x', 'o' ]


--uniformDistribution ∷ PDF Char
--uniformDistribution = createPDF $ (1.0,) <$> (grassTiles <> rockTiles <> flowerTiles)


testDistribution ∷ Palette → PDF Tile
testDistribution p = createPDF $   (0.6, Grass 0) :|
                                 [ (0.6, Grass 1)
                                 , (0.6, Grass 2)
                                 , (0.3, Rock  0)
                                 , (0.15, Flower 0)
                                 , (0.15, Flower 1)
                                 ]


-------------------------------------------------------------------------------

type Opened = Bool
type CursesDrawable = (Char, [C.Attribute])


data Tile
    = Wall   Direction
    | Door   Opened
    | Grass  Int
    | Rock   Int
    | Flower Int
    deriving (Eq, Ord, Show)


drawableFromTile ∷ Palette → Tile → CursesDrawable
drawableFromTile p (Wall North)     = ('═', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall NorthEast) = ('╗', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall East)      = ('║', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall SouthEast) = ('╝', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall South)     = ('═', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall SouthWest) = ('╚', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall West)      = ('║', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Wall NorthWest) = ('╔', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Door True)      = ('/', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Door False)     = ('+', [ C.AttributeColor (yellow p), C.AttributeDim ])
drawableFromTile p (Grass ix@0)     = (grassTiles !! ix, [ C.AttributeColor (green p), C.AttributeDim ])
drawableFromTile p (Grass ix@1)     = (grassTiles !! ix, [ C.AttributeColor (green p), C.AttributeDim ])
drawableFromTile p (Grass ix@2)     = (grassTiles !! ix, [ C.AttributeColor (green p) ])
drawableFromTile p (Rock ix@0)      = (rockTiles !! ix, [ C.AttributeColor (white p), C.AttributeDim ])
drawableFromTile p (Flower ix@0)    = (flowerTiles !! ix, [ C.AttributeColor (red p), C.AttributeDim ])
drawableFromTile p (Flower ix@1)    = (flowerTiles !! ix, [ C.AttributeColor (blue p), C.AttributeDim ])
drawableFromTile _ _                = error "Tile with invalid setup!"

-------------------------------------------------------------------------------

data Character a = Character {
      _charName  ∷ String
    , _inventory ∷ [a]
    }
    deriving (Show)
makeLenses ''Character


-- TODO Reeks of state monad
addToInventory ∷ a → Character a → Character a
addToInventory i = inventory <>~ [i]


removeFromInventory ∷ (Eq a) ⇒ a → Character a → Character a
removeFromInventory i = inventory %~ delete i

-------------------------------------------------------------------------------

data Entity a = Entity {
      _position ∷ V2 Int   -- <-- Do we want negative positions?
    , _object   ∷ a  -- Expand to something else, rather than just drawable data
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


-- TODO reeks of State monad (you're in the know, right?)
update ∷ Event
       → Game (Character CursesDrawable) CursesDrawable c Tile
       → Game (Character CursesDrawable) CursesDrawable c Tile
update (Move d)   og = displayCharacterStatus $ moveAvatar d og
update Attack     og = og
update Open       og = og
update Close      og = og
update Get        og = displayCharacterStatus $ pickUpItem (og ^. avatar.position) (Grass 0) og
update Talk       og = og
update Idle       og = og
update Quit       og = running .~ False $ og


displayCharacterStatus ∷ (Show a) ⇒ Game a b c d → Game a b c d
displayCharacterStatus g = debugStatus .~ views avatar show g $ g


-- Do change only the entity, not the game, like this:
moveAvatar ∷ Direction → Game a b c Tile → Game a b c Tile
moveAvatar d og = let op  = og ^. avatar.position
                      np  = fmap (max 0) $ op + directionToVector d
                      nap = fromMaybe op $ do
                                mapTile ← (og ^. map.mapData) V.!? fromIntegral (coord2Linear np (og ^. map.width))
                                case mapTile of
                                    (Wall d)  → pure op
                                    (Door  t) → pure $ bool op np t
                                    _         → pure np
                  in  avatar .~ Entity nap (og ^. avatar.object) $ og


pickUpItem ∷ (Eq b, b ~ d) => V2 Int → Tile → Game (Character b) b c Tile → Game (Character b) b c Tile
pickUpItem v rv og = fromMaybe og $ do
    obj ← find ((==v) . view position) (og ^. items)
    pure $
        flip execState og $ do
            avatar.object %= addToInventory (obj ^. object)
            items         %= delete obj
            map.mapData   %= (V.// [(fromIntegral $ coord2Linear v (og ^. map.width), rv)])


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

render ∷ Palette → Game a CursesDrawable c Tile → C.Curses ()
render p g = do
    w ← C.defaultWindow
    C.updateWindow w renderAction
    C.render
    where
        renderAction = do
            drawMap p (g ^. map)
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

drawMap ∷ Palette → TileMap Tile → C.Update ()
drawMap p m = mapM_ drawTile . zip [0..] . V.toList . view mapData $ m
    where
        drawTile ∷ (Int, Tile) → C.Update ()
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord (fromIntegral i) (m ^. width)
            C.moveCursor (fromIntegral y) (fromIntegral x)
            C.drawGlyph (uncurry C.Glyph (drawableFromTile p t))


drawItems ∷ [Entity CursesDrawable] → C.Update ()
drawItems = traverse_ drawItem
    where
        drawItem (Entity (V2 x y) cd) = do
            C.moveCursor (fromIntegral y) (fromIntegral x)
            C.drawGlyph (uncurry C.Glyph cd)



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
    m ← liftIO (MG.generate c r (Grass 0)  (testDistribution p))

    gameLoop p (initialGame p m)
    where
        initialChar   = Entity (V2 0 0) $ Character "John" []

        initialGame ∷ Palette → TileMap Tile → Game (Character CursesDrawable) CursesDrawable CursesDrawable Tile
        initialGame p m = Game True initialChar (createItemsFromMap p m) [] m ""

        gameLoop ∷ Palette → Game (Character CursesDrawable) CursesDrawable CursesDrawable Tile → C.Curses ()
        gameLoop p g = do
            e ← nextEvent
            let ng = update e g
            render p ng
            when (ng ^. running) $
                gameLoop p ng

        preparePalette = pure Palette
                         <*> C.newColorID C.ColorBlack   C.ColorBlack 1
                         <*> C.newColorID C.ColorRed     C.ColorBlack 2
                         <*> C.newColorID C.ColorGreen   C.ColorBlack 3
                         <*> C.newColorID C.ColorYellow  C.ColorBlack 4
                         <*> C.newColorID C.ColorBlue    C.ColorBlack 5
                         <*> C.newColorID C.ColorMagenta C.ColorBlack 6
                         <*> C.newColorID C.ColorCyan    C.ColorBlack 7
                         <*> C.newColorID C.ColorWhite   C.ColorBlack 8

        -- TODO fix this, don't create entities here, but elsewhere?
        createItemsFromMap ∷ Palette → TileMap Tile → [Entity CursesDrawable]
        createItemsFromMap p m =
            let createEntityFromTile ix =
                    Entity
                        (fromIntegral <$> linear2Coord (fromIntegral ix) (m ^. width))
                getAllEntities ix t l = createEntityFromTile ix (drawableFromTile p t) : l
            in  V.ifoldr' getAllEntities [] . view mapData $ m

