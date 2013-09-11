Distributed SDE solver
======================

This is a program which evaluates Stochastic Differential Equations (SDEs) distributed over multiple multi-core nodes.

The built-in combinators can be used as a toolbox providing utilities for solving different kind of equations,
but also in a lot of different ways. For example you can evaluate in any monad or using any suitable RNG.

Equations are given on a standard format and then solved using the /withSolver/ function,
resulting in a scalar value at the end of the interval.

Included in the package are ways of doing distributed calculations over an MPI cluster,
or optionally only using the local solver with built in parallelization.
Two SDE instances have been implemented; geometric brownian motion and the Langevin equation.
See the haddock documentation of the DSDE module for examples.

The main interface is accessible through Numeric.DSDE which provides various way of solving generic problems.
This module supports either local or distributed calculations in the IO monad and gathering the results as a distribution.
Under the surface there is also a working pure implementation for monadic environments, using a pure mersenne twister PRNG.

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
