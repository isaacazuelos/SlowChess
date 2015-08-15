-- |
-- Module      : Game.SlowChess.Evaluate
-- Description : Scoring chess games.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Chess game evaluation. Evaluating a game assigns the game a score based
-- on how well the current player is doing. The goal is to get as good an
-- estimate as possible /without/ looking ahead.

module Game.SlowChess.Evaluate (eval) where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.Board
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Mask hiding (fromList)
import           Game.SlowChess.Piece

-- | Evaluate a game. Right now it's a pretty simple pice counting.
-- Values taken from https://chessprogramming.wikispaces.com/Point+Value
eval :: Game -> Score
eval g -- if checkmate g then 1/0 else countPieces g
    | checkmate g = 1/0
    | check     g = 2500
    | draw      g = -500
    | otherwise   = (heatmapCount c b + plyBonus g)/heatmapCount (enemy c) b
  where c = player g
        b = board g

-- | Count the number of pieces, and give each a score. See 'eval' for
-- details.
{-# INLINE heatmapCount #-}
heatmapCount :: Colour -> Board -> Score
heatmapCount c b = counts
  where counts = (100.0  * heatMap (get c Pawn b))
               + (350.0  * heatMap (get c Knight b))
               + (350.0  * heatMap (get c Bishop b))
               + (525.0  * heatMap (get c Rook b))
               + (1000.0 * heatMap (get c Queen b))

-- | The idea here is that certain types of movements can be prefered. We
-- might prefer to move bishops over moving the king, for example.
{-# INLINE plyBonus #-}
plyBonus :: Game -> Score
plyBonus g = case ply g of
    Just (Move _ King _ _)   -> -10.0
    Just (Move {})           -> 10.0
    Just (Promotion {})      -> 100.0
    Just (Castle _ _)        -> 100.0
    Nothing                  -> 0.0
    _                        -> 0.0

-- | Heatmap assigns a weighted score according to the positions on a mask.
{-# INLINE heatMap #-}
heatMap :: Mask -> Score
heatMap m = (1.0 *  fromIntegral (count (both aMask m))) -- 1.15
          + (1.0 *  fromIntegral (count (both bMask m))) -- 1.10
          + (1.0 *  fromIntegral (count (both cMask m))) -- 1.05
          + (1.0 *  fromIntegral (count (both dMask m))) -- 1.00
          + (1.0 *  fromIntegral (count (both eMask m))) -- 0.90
          + (1.0 *  fromIntegral (count (both fMask m))) -- 0.80
  where aMask = Mask 103481868288
        bMask = Mask 26543503441920
        cMask = Mask 39866996097024
        dMask = Mask 142393223479296
        eMask = Mask 35465847065574912
        fMask = Mask 18410996751667331583
