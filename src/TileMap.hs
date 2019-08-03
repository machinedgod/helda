{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module TileMap
( Width
, Height
, TileMap(TileMap)
, name
, width
, height
, mapData

, loadFromFile

, linear2Coord
, coord2Linear
) where


import Control.Lens            (makeLenses)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Linear (V2(V2))

import qualified Data.Vector as V (Vector, fromList)

--------------------------------------------------------------------------------

type Width  = Word
type Height = Word

--------------------------------------------------------------------------------

data TileMap a = TileMap {
      _name    ∷ String
    , _width   ∷ Width
    , _height  ∷ Height
    , _mapData ∷ V.Vector a
    }
makeLenses ''TileMap


loadFromFile ∷ (MonadIO m) ⇒ FilePath → (Char → a) → m (TileMap a)
loadFromFile f ef = liftIO (stringToMap <$> readFile f)
    where
        stringToMap = let w = fromIntegral . length . takeWhile (/=('\n'))
                          h = fromIntegral . (+1) . length . filter (==('\n'))
                          d = V.fromList .  fmap ef . filter (/=('\n'))
                      in  TileMap f <$> w <*> h <*> d


linear2Coord ∷ Word → Width → V2 Word
linear2Coord i w = let x = i `mod` w
                       y = i `div` w
                   in  V2 x y


coord2Linear ∷ V2 Int → Width → Word
coord2Linear (V2 x y) w = fromIntegral (max 0 (y * fromIntegral w + x))

