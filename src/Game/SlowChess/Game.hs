-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model. This is the module responsible for building
-- 'Game' trees, and exporting only ways of manipulating them that remain
-- completely legal according the rules of chess.

module Game.SlowChess.Game ( -- * Constructors
                             Game
                           , start
                           , challange
                           , toDepth
                           ) where

import           Game.SlowChess.Board          hiding (update)
import           Game.SlowChess.Game.Fifty
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.EnPassant
import           Game.SlowChess.Piece

-- | The starting game.
start :: Game
start = enableAllCastleOptions (challange White starting)

-- | Builds a challange board. Castling isn't enabled, but you can change
-- that with 'enableAllCastleOptions'.
challange :: Colour -> Board -> Game
challange c b = update (unfoldFuture legal g)
  where g = Game { player = c
                 , status = defaultStatus
                 , board  = b
                 , ply    = Nothing
                 , future = error "future unititialied."
                 }

-- | Build the complete tree to a specific depths, after which 'future' is
-- always empty.
toDepth :: Int -> Game -> Game
toDepth 0 g = g { future = [] }
toDepth n g = g { future = map (toDepth (pred n)) (future g) }

-- | Generate the moves which might be legal, apart from the rules checked in
-- 'updates'.
genMoves :: Game -> [Game]
genMoves g = moves g ++ castle g ++ enPassant g

-- | Do all the updates. This means computing the checkmate, check, and fifty
-- move rule status of a game. Note that updates like this are computed
-- /before/ the turn is technically over, so the colour of the player hasn't
-- moved over yet.
update :: Game -> Game
update = updateCheckmate . updateCheck . fiftyMoveRule

-- | All legal futures for a game. This applies /all/ of the rules. It doesn't
-- really deal with draws though.
legal :: Game -> [Game]
legal = filter (not . check) .  map (finishTurn . update) . genMoves

-- | Is the game won? A game is in checkmate if the current player's king will
-- remain in check regardless of how they play.
updateCheckmate :: Game -> Game
updateCheckmate g@(Game { board = b, player = c }) = setCheckmate g cm
  where cm = check g && kingMovements `submask` attackable
        kingMovements = targets (moveKings c b)
        attackable = attacks (enemy c) (wipe b (get c King b))

-- | Compute (and store in the game) if a game is in check if the current
-- player's king could be captured if they do nothing.
updateCheck :: Game -> Game
updateCheck g@(Game { board = b, player = c }) = setCheck g isInCheck
  where kingPosition = get c King b
        isInCheck = kingPosition `submask` attacks (enemy c) b
                        && kingPosition /= 0
