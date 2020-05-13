{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PDF
( PDF, createPDF
, CDF, fromPdf, pickElement
) where

import Control.Applicative  (Alternative(..))
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Newtype      (Newtype(..))
import Data.Bifunctor       (first)
import Data.Foldable        (foldl')
import Data.List.NonEmpty   (NonEmpty((:|)), sortBy)
import Data.Maybe           (fromJust)

--------------------------------------------------------------------------------

newtype PDF a
    = PDF (NonEmpty (Float, a))
    deriving (Show)

instance Newtype (PDF a) (NonEmpty (Float, a))


createPDF ∷ (Ord a) ⇒ NonEmpty (Float, a) → PDF a
createPDF l = PDF $ sortBy probabilityOrder $ fmap (first normalize) l
    where
        normalize v = v / sum (fst <$> l)
        probabilityOrder t1 t2 = fst t1 `compare` fst t2

--------------------------------------------------------------------------------

newtype CDF a
    = CDF (NonEmpty (Float, a))
    deriving (Show)

instance Newtype (CDF a) (NonEmpty (Float, a))


fromPdf ∷ PDF a → CDF a
fromPdf (unpack → updf) =
    let adj = foldl1 (+) (fst <$> updf)
        res = snd $ foldr (addEntry adj) (0.0, []) updf
     in CDF $ head res :| tail res
    where
        addEntry ∷ Float → (Float, a) → (Float, [(Float, a)]) → (Float, [(Float, a)])
        addEntry adj (f, x) (acc, l) = let y = acc + f * adj
                                        in (y, l <> [(y, x)])


pickElement ∷ (MonadRandom m) ⇒ CDF a → m a
pickElement cdf = do
    i ← getRandomR (0, 1)
    pure $
        fromJust $
            foldl' (findFirst i) empty (unpack cdf)
    where
        findFirst ∷ (Alternative al) ⇒ Float → al a → (Float, a) → al a
        findFirst i acc (p, el) = if i < p then acc <|> pure el else acc

