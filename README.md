Distributed SDE solver
======================

This package contains utilities for solving SDE instances in various ways.
Basically an SDE instance is solved using some SDESolver working with some distribution mechanism.
Results are gathered at the end point of the specified interval.

Included in the package are ways of doing distributed calculations over an MPI cluster,
or optionally only using the local solver with built in parallelization.
Two SDE instances have been implemented; geometric brownian motion and the Langevin equation.
See the haddock documentation of the DSDE module for examples.

The main interface is accessible through Numeric.DSDE which provides various way of solving generic problems.
This module supports either local or distributed calculations in the IO monad and gathering the results as a distribution.
Under the surface there is also a working pure implementation for monadic environments, using a pure mersenne twister PRNG.

The code is written with a custom haskell-unicode plugin for vim, replacing math-stuff with unicode symbols, hence partial differentials and similar might look a little unappetizing.

Example
-------

    let r = 1.0
        sigma = 0.1
        y_0 = 3.0
        end = 0.2
        stepSize = 0.01
        runs = 1000
    withSolver Milstein (distribute (Langevin r sigma) y_0 end stepSize runs) ⤜
    writeResult "out"
