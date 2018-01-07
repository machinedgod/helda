{-# LANGUAGE UnicodeSyntax, NegativeLiterals #-}

module Direction
( Direction(getVector)
, north
, south
, east
, west
) where


import Linear

--------------------------------------------------------------------------------

newtype Direction = Direction { getVector ∷ V2 Int }
                  deriving(Show)


north ∷ Direction
north = Direction (V2 0 -1)

south ∷ Direction
south = Direction (V2 0 1)

east ∷ Direction
east = Direction (V2 1 0)

west ∷ Direction
west = Direction (V2 -1 0)

