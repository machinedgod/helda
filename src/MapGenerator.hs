{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module MapGenerator
where


import Control.Monad.Random (MonadRandom)

import qualified Data.Vector as V (fromList)

import TileMap
import PDF

--------------------------------------------------------------------------------

generate ∷ (MonadRandom m) ⇒ Width → Height → PDF a → m (TileMap a)
generate w h (fromPdf → cdf) = do
    gd ← traverse (const (pickElement cdf))  [0..w * h - 1]
    pure $ TileMap "generated" w h (V.fromList gd)
