{-# LANGUAGE UnicodeSyntax, NegativeLiterals, LambdaCase #-}

module Input
( getInput
) where

import Data.Maybe (fromJust)
import Data.Bool  (bool)

import qualified UI.NCurses as C

import Direction

--------------------------------------------------------------------------------

data Input = Idle
           | Move Direction
           | Attack
           | Open
           | Close
           | Get
           | Talk
           deriving (Show)


getInput ∷ C.Curses Input 
getInput = let mif = fmap (event2Input . fromJust) . (`C.getEvent` Nothing)
           in  C.defaultWindow >>= mif >>= \case
                                             Just i  → pure i
                                             Nothing → getInput
    where
        event2Input (C.EventCharacter 'k') = Just $ Move north
        event2Input (C.EventCharacter 'j') = Just $ Move south
        event2Input (C.EventCharacter 'l') = Just $ Move east
        event2Input (C.EventCharacter 'h') = Just $ Move west
        event2Input (C.EventCharacter 'f') = Just $ Attack
        event2Input (C.EventCharacter 'o') = Just $ Open
        event2Input (C.EventCharacter 'c') = Just $ Close
        event2Input (C.EventCharacter 'g') = Just $ Get
        event2Input (C.EventCharacter 't') = Just $ Talk
        event2Input (C.EventCharacter '.') = Just $ Idle
        event2Input _                      = Nothing
