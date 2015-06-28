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
* ~~Sketch a concept of a *game*~~
* ~~Special piece movement.~~
    * ~~Castling~~
    * ~~Promotion~~
    * ~~*En passant*~~
* Sketch tools for working with game trees.
* Game AI — Negamax?
* Game UI — The `/cli/` directory and executable are for this.
* Algebraic Chess Notation support — It might be nice to manipulate games.
* Profiling — I'd like to learn more about profiling in Haskell.
* Benchmarking — I'd also like to try to set up some benchmarks.

### To Fix

* Make `Game.SlowChess.Board` use `Coord`s.
* Add tests for modules once the interfaces solidify a bit.
