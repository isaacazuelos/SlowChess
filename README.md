SlowChess
=========

A chess engine in Haskell with no great emphasis on anything.

It's still *very* early in development, so there's nothing much to see.

TODO
----

### To Do

* Basic piece movement
* Build a concept of a *game*.
* Special piece movement.
* Game AI — Negamax?
* Game UI — The `/cli/` directory and executable are for this.
* Algebraic Chess Notation support — It might be nice to manipulate games.
* Profiling — One of purposes of this project is to demystify Haskell
  performance.

### To Fix

* Extend the documentation for `Game.SlowChess.Movement.movePawn` with links
  to where special pawn movement is documented.

### Things to fix when I have an internet connection again:

* Look up how to fix the documentation in `Game.SlowChess.Mask` for movement.
* Look into using lenses for the board representation access.

