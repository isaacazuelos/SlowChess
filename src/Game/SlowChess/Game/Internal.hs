-- |
-- Module      : Game.SlowChess.Game.Internal
-- Description : A game state in an unsafe way.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model presented with the implementation details
-- exposed in such a way as to allow for the creating and manipulation
-- of otherwise illegal states.
--
-- This module exists for two reasons.
--
-- 1. It allows for developers to get their hands dirty with the
--    implementation details if they wish, allowing for the creation
--    of replacements of 'Game.SlowChess.Game' that might extend or
--    modify the rules.
--
-- 2. It allows us to avoid the cyclical dependency issues that arise
--    from the existence of the /special movements/ as described in
--    'Game.SlowChess.Movement' --- where moves require game
--    knowledge, but to preserve the consistency of the game, valid
--    movement knowledge is required.

module Game.SlowChess.Game.Internal where

import           Game.SlowChess.Board
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- | A raw game state. Since this is still being sketched, this is
-- likely to change a lot over the next few commits.
data Game = Game { player       :: Colour           -- ^ current player
                 , board        :: Board            -- ^ current board
                 , ply          :: Maybe Ply        -- ^ last ply played
                 , past         :: Maybe Game       -- ^ previous game states
                 , _future      :: Maybe [Game]
                   -- Rule-specific requirements.
                 , castleStatus :: [(Colour, Side)] -- ^ available casltes
                 , drawStatus   :: DrawStatus       -- ^ three fold rule
                 , fiftyStatus  :: Int              -- ^ fifty move rule
                 } deriving ( Eq )

-- | Prints a bunch of information about a Game's state, Information about
-- the future is left out to prevent
instance Show Game where
  show g = "\nplayer: "  ++ show (player g)      ++
           "\nply: "     ++ show (ply g)         ++
           "\nfifty: "   ++ show (fiftyStatus g) ++
           "\ndraw: "    ++ show (drawStatus g)  ++
           "\nboard:"    ++ show (board g)       ++
           "\nfutures: " ++ case _future g of
                                Nothing -> "not set"
                                Just fs -> show $ length fs


-- | A generic version of the next game, updating the player and past in the
-- obvious way.
next :: Game -> Ply -> Board -> Game
next g p b = g { player     = enemy (player g)
               , board      = b
               , ply        = Just p
               , past       = Just g
               , drawStatus = Normal
               }

-- | Build out the future, recursivly. The 'Rule' passed in is used to
-- generate all possible futures.
buildFutureBy :: Rule -> Game -> Game
buildFutureBy f g = g { _future = Just $ map (buildFutureBy f) (f g) }

-- | The history of a game, back to the first 'Game'.
history :: Game -> [Game]
history g = case past g of
              Just g' -> g' : history g'
              Nothing -> []

-- | Toggles the current player of a game.
togglePlayer :: Game -> Game
togglePlayer g = g { player = enemy $ player g }

-- | A rule is something which either grows or restricts the possible futures
-- of a 'Game'.
type Rule = Game -> [Game]

-- | Track if a game is drawn, or if a draw was just offered.
data DrawStatus = Claimable | Normal | Offered deriving ( Show, Eq )
