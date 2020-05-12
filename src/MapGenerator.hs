{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MapGenerator
where


import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Maybe           (isNothing, fromJust)
import Data.List.NonEmpty   (NonEmpty((:|)))
import Data.Foldable        (foldl')

import qualified Data.Vector as V (fromList)

import TileMap
import PDF

--------------------------------------------------------------------------------

generate ∷ ∀ m a. (MonadRandom m) ⇒ Width → Height → PDF a → m (TileMap a)
generate w h pdf = do
    gd ← traverse (const pickCharacter)  [0..w * h - 1]
    pure $ TileMap "generated" w h (V.fromList gd)
    where
        pickCharacter ∷ m a
        pickCharacter = do
            i ← getRandomR (0, 1)
            pure $
                fromJust $ -- TODO CRASH
                    foldl' (findFirst i) Nothing (toCdf pdf)
        
        findFirst ∷ Float → Maybe a → (Float, a) → Maybe a
        findFirst i acc (p, el) = if i > p && isNothing acc
                                    then pure el
                                    else Nothing

toCdf ∷ PDF a → NonEmpty (Float, a)
toCdf (unwrapPDF → updf) =
    let adj = foldl1 (+) (fst <$> updf)
        res = snd $ foldl' (addEntry adj) (0.0, []) updf
     in head res :| tail res
    where
        addEntry ∷ Float → (Float, [(Float, a)]) → (Float, a) → (Float, [(Float, a)])
        addEntry adj (acc, l) (f, x) = let y = acc + f * adj
                                        in (y, l <> [(y, x)])

