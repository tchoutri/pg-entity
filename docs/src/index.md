## Documentation for the `pg-entity` library

Welcome to [`pg-entity`][pg-entity]'s documentation.

Read the Tutorial to learn how to use the library

See also these resources to understand the library's underlying mechanisms:

* [postgresql-simple][postgresql-simple]: The library that provies the Query primitives and the database connection 
* [pg-transact][pg-transact]: The library that provides the transaction monad transformer in which our queries run
* Oliver Charles' *24 Days of GHC Extensions* series if you want to understand the underlying derivation mechanisms that enable us to write less code:
    * [Deriving](https://ocharles.org.uk/guest-posts/2014-12-15-deriving.html)
    * [DeriveGeneric](https://ocharles.org.uk/posts/2014-12-16-derive-generic.html)
* Richard Eisenberg's [*Avoid boilerplate instances with -XDerivingVia*][derivingvia] is most helpful to understand how the `GenericEntity` mechanism
  works, as well as a showcase for other deriving strategies.

[pg-entity]: https://github.com/tchoutri/pg-entity
[postgresql-simple]: https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple.html
[pg-transact]: https://hackage.haskell.org/package/pg-transact/docs/Database-PostgreSQL-Transact.html
[derivingvia]: https://www.youtube.com/watch?v=UZaQuSIrO6s

