A Riemann Client for Haskell
============================

[![Build Status](https://travis-ci.org/tel/riemann-hs.png?branch=master)](https://travis-ci.org/tel/riemann-hs)

Nothing too fancy here yet. I might call this `Zeta` as well if it's not too
confusing with previous designs at https://github.com/reifyhealth/zeta.

Todos
-----

* Batched event transmissions
    * Using a worker thread
    * Using an efficient builder
* Msg delivery over TCP
* `RiemannT` API design
    * Unified API for UDP/asynch/cheap messages versus TCP/synch/guaranteed messages
* Background thread deliving RTS events to a client
* Better tests and documentation
* Eliminate the `lens` dependency (perhaps?)
* Query language support (see branch dev-queries)
    * Parser/PrettyPrinter combination to form an `iso` on the `HasQuery` class
