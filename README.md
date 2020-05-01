# helda
Code for "Make you a roguelike in Haskell for greater good" youtube series.

Clone, then run `stack install` and off you go.


# Upcoming

### Basic game
- Applicative instance and Monad instance for Entity and why are instances of
  common typeclasses important (reusable knowledge)
- either monad transformer stack or 'runThisEntity' function in the game API
- Dyre for hotswapping
- multiple screens to walk to
- screen re-generation from seed + delta list
- gameplay (alchemy, obstacles, etc) - ask people for suggestions
- design game systems alchemy by applying cat. theory - propose an idea that
  composable, coherent, closed systems have an inherent fun factor and are
  related to emergent systems

### Nice features
- simplest 2d raycasting
- fog of war
- game data in QQs (procgen,items,monsters,etc)

### Portability
- rendering api separation
- ascii backend
- 2d (sdl?) backend
- GL backend
- shader linking correctness with types, write shaders with QQ, check if
  there's stuff on hackage already
- portable code (ncurses probably isn't very portable on win, even with
                 mingw/cygwin, we want native stuff)


### ...? (open to proposals)
