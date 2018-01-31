{-# LANGUAGE UnicodeSyntax, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module MapGenerator
where

import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Semigroup       ((<>))
import Data.Bifunctor       (first)
import Data.List            (findIndex)
import Data.Foldable        (foldl')

import qualified Data.Vector as V (fromList)

import TileMap
import PDF

--------------------------------------------------------------------------------

grassTiles ∷ String
grassTiles = [ ',', '"', '\'' ]


rockTiles ∷ String
rockTiles = [ '.' ]


uniformDistribution ∷ PDF Char
uniformDistribution = createPDF $ (1.0,) <$> (grassTiles <> rockTiles)


testDistribution ∷ PDF Char
testDistribution = createPDF $ [ (1.0, ',')
                               , (0.8, '\'')
                               , (1.0, '"')
                               , (0.3, '.')
                               ]


generate ∷ (MonadRandom m) ⇒ Width → Height → (Char → a) → m (TileMap a)
generate w h f = do
    gd ← traverse (const (pickCharacter testDistribution))  [0..w * h]
    let d = fmap f gd
    pure $ TileMap "generated" w h (V.fromList d)
    where
        pickCharacter ∷ (MonadRandom m) ⇒ PDF Char → m Char
        pickCharacter pdf = do
            i  ← getRandomR (0, 1)
            pure $ case findIndex ((>i) . fst) (toCdf pdf) of
                Just idx → snd (unwrapPDF pdf !! idx)
                Nothing  → '?'


toCdf ∷ PDF a → [(Float, a)]
toCdf (unwrapPDF → [])   = error "Empty PDF!"
toCdf (unwrapPDF → updf) = snd $ foldl' (\(acc, l) (f, x) → (acc + f, l ++ [(acc + f, x)]))  (0.0, []) updf 

