{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module MapGenerator
where


import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Semigroup       ((<>))
import Data.List            (findIndex)
import Data.List.NonEmpty   (NonEmpty((:|)), toList)
import Data.Foldable        (foldl', foldl1)

import qualified Data.Vector as V (fromList)

import TileMap
import PDF

--------------------------------------------------------------------------------

-- TODO don't meddle with chars, and take in distribution and datatype
generate ∷ (MonadRandom m) ⇒ Width → Height → a → PDF a → m (TileMap a)
generate w h emptyEl pdf = do
    gd ← traverse (const (pickCharacter pdf emptyEl))  [0..w * h - 1]
    pure $ TileMap "generated" w h (V.fromList gd)
    where
        pickCharacter ∷ (MonadRandom m) ⇒ PDF a → a → m a
        pickCharacter pdf emptyEl = do
            i  ← getRandomR (0, 1)
            pure $
                case findIndex ((>i) . fst) (toCdf pdf) of
                    Just idx → snd (toList (unwrapPDF pdf) !! idx)
                    Nothing  → emptyEl


-- TODO change output type into NonEmpty
toCdf ∷ PDF a → [(Float, a)]
toCdf (unwrapPDF → updf) =
    let adj = foldl1 (+) (fst <$> updf)
     in snd $ foldl' (addEntry adj) (0.0, []) updf
    where
        addEntry ∷ Float → (Float, [(Float, a)]) → (Float, a) → (Float, [(Float, a)])
        addEntry adj (acc, l) (f, x) = let y = acc + f * adj
                                        in (y, l <> [(y, x)])

