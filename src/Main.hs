{-# LANGUAGE UnicodeSyntax, DeriveFunctor, NegativeLiterals #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad          (when)

import Input
import Linear

import qualified UI.NCurses as C

-------------------------------------------------------------------------------

main ∷ IO ()
main = C.runCurses $ do
    C.setCursorMode C.CursorInvisible
    C.setEcho       False
    gameLoop initialGame
    where
        initialGame = Game True (pure Character) [] [] TileMap
        gameLoop g = do
            e ← nextEvent
            let ng = update e g
            --liftIO $ putStrLn $ "Char: " ++ show (avatar ng)
            render ng

            when (running ng) $
                gameLoop ng
        render ng = do
            dw ← C.defaultWindow
            C.updateWindow dw $ do
                let spaceGlyph = C.Glyph '.' []
                let pcGlyph    = C.Glyph '@' [ C.AttributeBold ]
                let (V2 x y) = position $ avatar ng
                C.clear
                C.moveCursor (fromIntegral y) (fromIntegral x) 
                C.drawGlyph pcGlyph
            C.render

    
--------------------------------------------------------------------------------

data Entity a = Entity {
      position ∷ V2 Int   -- <-- Do we want negative positions?
    , object   ∷ a
    }
    deriving (Show, Functor)


instance Applicative Entity where
    pure = Entity (V2 0 0)
    (Entity _ f) <*> e = fmap f e 

instance Monad Entity where
    (Entity _ v) >>= f = f v

--------------------------------------------------------------------------------

data Character = Character deriving(Show)
data Item      = Item      deriving(Show)
data TileMap   = TileMap   deriving(Show)


data Game = Game {
      running ∷ Bool

    , avatar  ∷ Entity Character
    , items   ∷ [Entity Item]
    , enemies ∷ [Entity Character]

    , map     ∷ TileMap
    }


update ∷ Event → Game → Game
update (Move d) og = og { avatar = let olda = avatar og
                                   in  Entity (fmap (max 0) $ position olda + directionToVector d) (object olda) }
update Attack   og = og
update Open     og = og
update Close    og = og
update Get      og = og
update Talk     og = og
update Idle     og = og

update Quit     og = og { running = False }


directionToVector ∷ Direction → V2 Int
directionToVector North = V2  0 -1
directionToVector South = V2  0  1
directionToVector East  = V2  1  0
directionToVector West  = V2 -1  0

