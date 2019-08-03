{-# LANGUAGE UnicodeSyntax #-}

module PDF
( PDF(unwrapPDF)
, createPDF
) where


import Data.List      (sortBy)
import Data.Bifunctor (first)

--------------------------------------------------------------------------------

newtype PDF a = PDF { unwrapPDF ∷ [(Float, a)] }
              deriving (Show)


createPDF ∷ (Ord a) ⇒ [(Float, a)] → PDF a
createPDF l = PDF $ sortBy probabilityOrder $ fmap (first normalize) l
    where
        normalize v = v / sum (fst <$> l)
        probabilityOrder t1 t2 = fst t1 `compare` fst t2

