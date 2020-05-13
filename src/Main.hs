{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE LambdaCase       #-}

module Main where


import Prelude hiding (map)

import Control.Lens           (makeLenses, (<>=), (.=), (^.), (%=),
                               view, use, uses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State    (MonadState, execState)
import Control.Monad          (when, void)
import Data.Bifunctor         (bimap)
import Data.Bool              (bool)
import Data.Foldable          (traverse_)
import Data.List              (delete, find)
import Data.List.NonEmpty     (NonEmpty((:|)))
import Data.Maybe             (fromMaybe)

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

-------------------------------------------------------------------------------

type Opened = Bool
type CursesDrawable = (Char, [C.Attribute])


class DrawableTile a where
    tileCharacter  ∷ a → Char
    tileAttributes ∷ Palette → a → [C.Attribute]

-------------------------------------------------------------------------------

data GrassTile
    = Comma
    | Quote
    | SQuote
    deriving (Eq, Ord, Show)

instance DrawableTile GrassTile where
    tileCharacter Comma  = ','
    tileCharacter Quote  = '"'
    tileCharacter SQuote = '\''

    tileAttributes p _  = [ C.AttributeColor (green p), C.AttributeDim ]



data RockTile
    = Fullstop
    deriving (Eq, Ord, Show)

instance DrawableTile RockTile where
    tileCharacter Fullstop = '.'
    tileAttributes p Fullstop = [ C.AttributeColor (white p), C.AttributeDim ]


data FlowerTile
    = RedFlower
    | BlueFlower
    | YellowFlower
    | MagentaFlower
    deriving (Eq, Ord, Show)

instance DrawableTile FlowerTile where
    tileCharacter RedFlower     = 'x'
    tileCharacter BlueFlower    = 'o'
    tileCharacter YellowFlower  = 'y'
    tileCharacter MagentaFlower = '*'

    tileAttributes p RedFlower     = [ C.AttributeColor (red p), C.AttributeDim ]
    tileAttributes p BlueFlower    = [ C.AttributeColor (blue p), C.AttributeDim ]
    tileAttributes p YellowFlower  = [ C.AttributeColor (yellow p), C.AttributeDim ]
    tileAttributes p MagentaFlower = [ C.AttributeColor (magenta p), C.AttributeDim ]


data Tile
    = Wall   Direction
    | Door   Opened
    | Grass  GrassTile
    | Rock   RockTile
    | Flower FlowerTile
    deriving (Eq, Ord, Show)

instance DrawableTile Tile where
    tileCharacter (Wall d) = case d of
        North     → '═'
        NorthEast → '╗'
        East      → '║'
        SouthEast → '╝'
        South     → '═'
        SouthWest → '╚'
        West      → '║'
        NorthWest → '╔'
    tileCharacter (Door o)    = bool '+'  '/' o
    tileCharacter (Grass gt)  = tileCharacter gt
    tileCharacter (Rock rt)   = tileCharacter rt
    tileCharacter (Flower ft) = tileCharacter ft

    tileAttributes p (Wall _)    = [ C.AttributeColor (yellow p), C.AttributeDim ]
    tileAttributes p (Door _)    = [ C.AttributeColor (yellow p), C.AttributeDim ]
    tileAttributes p (Grass gt)  = tileAttributes p gt
    tileAttributes p (Rock rt)   = tileAttributes p rt
    tileAttributes p (Flower ft) = tileAttributes p ft

-------------------------------------------------------------------------------

data Character i = Character {
      _charName  ∷ String
    , _inventory ∷ [i]
    }
    deriving (Show)
makeLenses ''Character


addToInventory ∷ (MonadState (Character a) ms) ⇒ a → ms ()
addToInventory i = inventory <>= [i]


removeFromInventory ∷ (Eq a, MonadState (Character a) ms) ⇒ a → ms ()
removeFromInventory i = inventory %= delete i

-------------------------------------------------------------------------------

data Entity a = Entity {
      _position ∷ V2 Int   -- <-- Do we want negative positions?
    , _object   ∷ a  -- Expand to something else, rather than just drawable data
    }
    deriving (Eq, Show, Functor)
makeLenses ''Entity

--------------------------------------------------------------------------------

data Game a i e t = Game {
      _running ∷ Bool

    , _avatar  ∷ Entity a
    , _items   ∷ [Entity i]
    , _enemies ∷ [Entity e]

    , _map     ∷ TileMap t

    , _debugStatus ∷ String
    }
makeLenses ''Game


update ∷ (MonadState (Game (Character Tile) Tile e Tile) ms) ⇒ Event → ms ()
update (Move d) = moveAvatar d *> displayCharacterStatus
update Attack   = pure ()
update Open     = pure ()
update Close    = pure ()
update Get      = do
    p ← use (avatar.position)
    pickUpItem p (Grass Comma)
    displayCharacterStatus
update Talk     = pure ()
update Idle     = pure ()
update Quit     = running .= False


displayCharacterStatus ∷ (Show a, MonadState (Game a i e Tile) ms) ⇒ ms ()
displayCharacterStatus = uses avatar show >>= (debugStatus .=)


-- Do change only the entity, not the game, like this:
moveAvatar ∷ (MonadState (Game a i e Tile) ms) ⇒ Direction → ms ()
moveAvatar d = do
    op ← use (avatar.position)
    ao ← use (avatar.object)

    md ← use (map.mapData)
    mw ← use (map.width)

    let np  = fmap (max 0) $ op + directionToVector d
        nap = fromMaybe op $ do
                     mapTile ← md V.!? fromIntegral (coord2Linear np mw)
                     case mapTile of
                         (Wall _) → pure op
                         (Door t) → pure $ bool op np t
                         _        → pure np
    avatar .= Entity nap ao


pickUpItem ∷ (Eq i, MonadState (Game (Character i) i e Tile) ms) => V2 Int → Tile → ms ()
pickUpItem v rv = do
    mw ← use (map.width)
    uses items (find ((==v) . view position)) >>= \case
        Nothing  → pure ()
        Just obj → do
            avatar.object %= execState (addToInventory (obj ^. object))
            items         %= delete obj
            map.mapData   %= (V.// [(fromIntegral $ coord2Linear v mw, rv)])


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

render ∷ (DrawableTile i) ⇒ Palette → Game a i e Tile → C.Curses ()
render p g = do
    w ← C.defaultWindow
    C.updateWindow w renderAction
    C.render
    where
        renderAction = do
            drawMap p (g ^. map)
            drawItems p (g ^. items)
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

drawMap ∷ (DrawableTile a) ⇒ Palette → TileMap a → C.Update ()
drawMap p m = mapM_ drawTile . zip [0..] . V.toList . view mapData $ m
    where
        drawTile ∷ (DrawableTile a) ⇒ (Int, a) → C.Update ()
        drawTile (i, t) = do
            let (V2 x y) = linear2Coord (fromIntegral i) (m ^. width)
            C.moveCursor (fromIntegral y) (fromIntegral x)
            C.drawGlyph (C.Glyph (tileCharacter t) (tileAttributes p t))


drawItems ∷ (DrawableTile i) ⇒ Palette → [Entity i] → C.Update ()
drawItems p = traverse_ drawItem
    where
        drawItem (Entity (V2 x y) t) = do
            C.moveCursor (fromIntegral y) (fromIntegral x)
            C.drawGlyph (C.Glyph (tileCharacter t) (tileAttributes p t))



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

testDistribution ∷ PDF Tile
testDistribution = createPDF $   (0.6, Grass Comma) :|
                               [ (0.6, Grass Quote)
                               , (0.6, Grass SQuote)
                               , (0.3, Rock  Fullstop)
                               , (0.10, Flower RedFlower)
                               , (0.15, Flower BlueFlower)
                               , (0.08, Flower YellowFlower)
                               , (0.03, Flower MagentaFlower)
                               ]


main ∷ IO ()
main = C.runCurses $ do
    void $ C.setCursorMode C.CursorInvisible
    C.setEcho False
    (r, c) ← bimap fromIntegral (fromIntegral . (1`subtract`)) <$> C.screenSize
    p ← preparePalette
    m ← liftIO (MG.generate c r testDistribution)

    gameLoop p (initialGame m)
    where
        initialChar ∷ Entity (Character Tile)
        initialChar = Entity (V2 0 0) $ Character "John" []

        initialGame ∷ TileMap Tile → Game (Character Tile) Tile Tile Tile
        initialGame m = Game True initialChar (createItemsFromMap m) [] m ""

        gameLoop ∷ Palette → Game (Character Tile) Tile Tile Tile → C.Curses ()
        gameLoop p g = do
            e ← nextEvent
            let ng = execState (update e) g
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
        createItemsFromMap ∷ TileMap Tile → [Entity Tile]
        createItemsFromMap m =
            let entityPositionFromIx ix = fromIntegral <$> linear2Coord (fromIntegral ix) (m ^. width)
                getAllEntities ix t l = Entity (entityPositionFromIx ix) t : l
            in  V.ifoldr' getAllEntities [] . view mapData $ m

