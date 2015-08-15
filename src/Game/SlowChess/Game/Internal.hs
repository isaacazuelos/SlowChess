{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
--
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

module Game.SlowChess.Game.Internal ( Game ( Game
                                           , player
                                           , board
                                           , status
                                           , ply
                                           , future
                                           )
                                    , GameStatus
                                    , defaultStatus
                                    , unfoldFuture
                                    , next
                                    , finishTurn
                                      -- * Access status flags
                                    , fifty
                                    , check
                                    , checkmate
                                    , draw
                                      -- * Set status flags
                                    , setFifty
                                    , setCheck
                                    , setCheckmate
                                    , setDraw
                                      -- Castling flags
                                    , castleOptions
                                    , enableAllCastleOptions
                                    , hasCastleOption
                                    , disableCastleOption
                                    ) where

import           Data.Bits
import           Data.Word

import           Game.SlowChess.Board
import           Game.SlowChess.Move.Internal (Ply)
import           Game.SlowChess.Piece

-- | The state of a game, where we explicitly store all the values
-- we compute as we compute them.
data Game = Game { player :: !Colour
                 , status :: {-# UNPACK #-} !GameStatus
                 , board  :: {-# UNPACK #-} !Board
                 , ply    :: Maybe Ply
                 , future :: [Game]
                 }

-- Games with the same board, status, and ply are equal. We don't need to
-- check the futures, since futures are dependent on the other values.
instance Eq Game where
    (==) a b = player a == player b
            && status a == status b
            && board  a == board  b
            && ply    a == ply    b

-- | Prints a bunch of information about a Game's state, Information about
-- the future is left out to prevent
instance Show Game where
  show g = "\nboard:"    ++ show (board  g) ++
           "\nplayer:"   ++ show (player g) ++
           "\nlast ply:" ++ show (ply    g) ++
           "\nstats:"    ++
           "\n  fifty move rule: " ++ show (fifty g)         ++
           "\n  castleOptions: "   ++ showCastleOptions      ++
           "\n  check: "           ++ show (check g)         ++
           "\n  checkmate: "       ++ show (checkmate g)     ++
           "\n  drawn: "           ++ show (draw g)
    where showCastleOptions = show options
          options = do c <- [White, Black]
                       s <- [Kingside, Queenside]
                       if hasCastleOption c s g
                           then return (c, s)
                           else []


-- | Build a game tree, using a given function to generate each game's future.
unfoldFuture :: (Game -> [Game]) -> Game -> Game
unfoldFuture f = go
  where go g = g { future = map go (f g) }

-- | Update most of the typical feilds in the obvious way.
next :: Game -> Ply -> Board -> Game
next g p b = g { player = player g
               , board  = b
               , ply    = Just p
               }

-- | Finish a turn by swapping the current player.
finishTurn :: Game -> Game
finishTurn g = g { player = enemy (player g)}

-- | GameFlags are various status bits for the game.
-- 01234567 89abcdef
-- ffffffff abcdCMD?
--
-- a = White Kingside  castle still legal
-- b = Black Kingside  castle still legal
-- c = White Queenside castle still legal
-- d = Black Queenside castle still legal
-- C = Check
-- M = Checkmate
-- D = Drawn game
-- ? = Unused
newtype GameStatus = Status Word16 deriving ( Bits, Eq, Show )

-- | A default game's status, where castling is disallowed, the game is not
-- in check, checkmate, drawn and doesn't have a draw available.
defaultStatus :: GameStatus
defaultStatus = Status 0

-- | A mask used to isolate the lower 8 bits used
fiftyMask :: Bits a => a
fiftyMask = foldl setBit zeroBits [0x0..0x7]

-- | The fifty move rule count of the game.
fifty :: Game -> Int
fifty g = intify (fiftyMask .&. status g)
  where intify (Status w) = fromIntegral w

-- | Just the bits which deal with castling options.
castleMask :: GameStatus
castleMask = Status (foldl setBit zeroBits [0x8, 0x9, 0xA, 0xB])

-- | Do two games have the same available castling options. This is only
-- really useful for comparing to other castling options.
castleOptions :: Game -> GameStatus
castleOptions g = castleMask .&. status g

-- | Is a castling option still available to a game?
hasCastleOption :: Colour -> Side -> Game -> Bool
hasCastleOption White Kingside  g = testBit (status g) 0x8
hasCastleOption Black Kingside  g = testBit (status g) 0x9
hasCastleOption White Queenside g = testBit (status g) 0xA
hasCastleOption Black Queenside g = testBit (status g) 0xB

-- | Disable a castle option.
disableCastleOption :: Colour -> Side -> Game -> Game
disableCastleOption White Kingside  g = setStatusBit 0x8 g False
disableCastleOption Black Kingside  g = setStatusBit 0x9 g False
disableCastleOption White Queenside g = setStatusBit 0xA g False
disableCastleOption Black Queenside g = setStatusBit 0xB g False

-- | Is a game in check?
check :: Game -> Bool
check g = testBit (status g) 0xC

-- | Is a game in checkmate?
checkmate :: Game -> Bool
checkmate g = testBit (status g) 0xD

-- | Is a game drawn?
draw :: Game -> Bool
draw g = testBit (status g) 0xE

-- | A generic bit setting function, which reaches inside a game to flip the
-- appropriate bits.
setStatusBit :: Int -> Game -> Bool -> Game
setStatusBit b g True  = g { status = setBit   (status g) b }
setStatusBit b g False = g { status = clearBit (status g) b }

-- | Set teh fifty move rule status of a game.
setFifty :: Game -> Int -> Game
setFifty g n = g { status = status' }
  where status' = Status (fromIntegral (fiftyMask .&. n))

-- | Set the check status of a game.
setCheck :: Game -> Bool -> Game
setCheck = setStatusBit 0xC

-- | Set if a game is in checkmate.
setCheckmate :: Game -> Bool -> Game
setCheckmate = setStatusBit 0xD

-- | Set if a game is a draw
setDraw :: Game -> Bool -> Game
setDraw = setStatusBit 0xE

-- | Enable all castle options, regardless of their current settings.
enableAllCastleOptions :: Game -> Game
enableAllCastleOptions g = g { status = status g .|. castleMask }
