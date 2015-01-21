SlowChess
=========

A chess engine in Haskell with no great emphasis on anything.

It's still *very* early in development, so there's nothing much to see.

To Do List
----------

### To Implement

* ~~Basic Chess Terms~~
* ~~Board Representation~~
* ~~Basic piece movement~~
* Build a concept of a *game*.
* Special piece movement.
* Game AI — Negamax?
* Game UI — The `/cli/` directory and executable are for this.
* Algebraic Chess Notation support — It might be nice to manipulate games.
* Profiling — I'd like to learn more about profiling in Haskell.
* Benchmarking — I'd also like to try to set up some benchmarks.

### To Fix

* Add tests for older modules
    * `Game.SlowChess.Movement` — Expand the tests to cover the piece-specific
      move functions against the rules in the documentation.
    * `Game.SlowChess.Mask` — There are a bunch of declared properties in the
	  documentation that could be tested with QuickCheck (mostly) easily.
    * `Game.SlowChess.Board`
	* `Game.SlowChess.Piece`, although I'm not sure there's much to test.
* Extend the documentation for `Game.SlowChess.Movement.movePawn` with links
  to where special pawn movement is documented.


