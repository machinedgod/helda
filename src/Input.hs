{-# LANGUAGE UnicodeSyntax, NegativeLiterals, LambdaCase #-}

module Input
( Direction(..)
, Event(..)

, nextEvent
) where

import Data.Maybe (fromJust)

import qualified UI.NCurses as C

--------------------------------------------------------------------------------

data Direction = North
               | South
               | East
               | West
               | NorthWest
               | NorthEast
               | SouthWest
               | SouthEast
               deriving (Eq, Ord, Show)


data Event = Idle
           | Move Direction
           | Attack
           | Open
           | Close
           | Get
           | Talk

           | Quit
           deriving (Show)


nextEvent ∷ C.Curses Event 
nextEvent = let mif = fmap (cursesEvent2GameEvent . fromJust) . (`C.getEvent` Nothing)
           in  C.defaultWindow >>= mif >>= \case
                                             Just i  → pure i
                                             Nothing → nextEvent
    where
        cursesEvent2GameEvent (C.EventCharacter 'k') = Just $ Move North
        cursesEvent2GameEvent (C.EventCharacter 'j') = Just $ Move South
        cursesEvent2GameEvent (C.EventCharacter 'l') = Just $ Move East
        cursesEvent2GameEvent (C.EventCharacter 'h') = Just $ Move West
        cursesEvent2GameEvent (C.EventCharacter 'y') = Just $ Move NorthWest
        cursesEvent2GameEvent (C.EventCharacter 'u') = Just $ Move NorthEast
        cursesEvent2GameEvent (C.EventCharacter 'b') = Just $ Move SouthWest
        cursesEvent2GameEvent (C.EventCharacter 'n') = Just $ Move SouthEast
        cursesEvent2GameEvent (C.EventCharacter 'f') = Just   Attack
        cursesEvent2GameEvent (C.EventCharacter 'o') = Just   Open
        cursesEvent2GameEvent (C.EventCharacter 'c') = Just   Close
        cursesEvent2GameEvent (C.EventCharacter 'g') = Just   Get
        cursesEvent2GameEvent (C.EventCharacter 't') = Just   Talk
        cursesEvent2GameEvent (C.EventCharacter '.') = Just   Idle

        cursesEvent2GameEvent (C.EventCharacter 'q') = Just   Quit
        cursesEvent2GameEvent _                      = Nothing
