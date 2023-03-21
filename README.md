<h1 align="center">
  pg-entity
</h1>

<p align="center">
<a href="https://github.com/tchoutri/pg-entity/actions">
  <img src="https://img.shields.io/github/actions/workflow/status/tchoutri/pg-entity/ci.yml?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
<a href="https://hackage.haskell.org/package/pg-entity">
  <img src="https://img.shields.io/hackage/v/pg-entity?style=flat-square" alt="Hackage" />
</a>
</p>

This library is a pleasant layer on top of [postgresql-simple][pg-simple] to safely expand the fields of a table when
writing SQL queries.  
It aims to be a convenient middle-ground between rigid ORMs and hand-rolled SQL query strings. Here is its philosophy:

* The serialisation/deserialisation part is left to the consumer, so you have to go with your own FromRow/ToRow instances.
  You are encouraged to adopt data types in your application that model your business domain, rather than restrict yourself within the limits of what
  an SQL schema can represent. Use an intermediate Data Access Object (DAO) that can easily be serialised and deserialised
  to and from a SQL schema, to and from which you will morph your business data-types.
* Illegal states are made harder (but not impossible) to represent. Generic deriving of entities is encouraged, and
  quasi-quoters are provided to denote fields in a safer way. 
* Escape hatches are provided at every level. The types that are manipulated are Query for which an `IsString` instance exists.
  Don't force yourself to use the higher-level API if the lower-level combinators work for you, and if those don't either, “Just Write SQL”™.

Its dependency footprint is optimised for my own setups, and as such it makes use of [text][text], [vector][vector] and
[pg-transact][pg-transact].



Table of Contents
=================

* [Installation](#installation)
* [Documentation](#documentation)
  * [Escape hatches](#escape-hatches)
* [Acknowledgements](#acknowledgements)

## Installation

At present time, `pg-entity` is published on Hackage but not on Stackage. To use it in your projects, add it in your
cabal file like this:

```
pg-entity ^>= 0.0
```

or in your `stack.yaml` file:

```
extra-deps:
  - pg-entity-0.0.4.0
```

* [List of supported GHC versions](https://github.com/tchoutri/pg-entity/blob/main/pg-entity.cabal#L16)

## Documentation

* [Tutorial][docs-url]

This library aims to be thoroughly tested, by the means of Oleg Grenrus' [cabal-docspec][docspec]
and more traditional tests for database roundtrips.

I aim to produce and maintain a decent documentation, therefore do not hesitate to raise an issue if you feel that
something is badly explained and should be improved.


### Escape hatches

Safe SQL generation is a complex subject, and it is far from being the objective of this library. The main topic it
addresses is listing the fields of a table, which is definitely something easier. This is why every level of this wrapper
is fully exposed, so that you can drop down a level at your convience.

It is my personal belief, firmly rooted in experience, that we should not aim to produce statically-checked SQL and have
it "verified" by the compiler. The techniques that would allow that in Haskell are still far from being optimised and
ergonomic. As such, this library makes no effort to produce semantically valid SQL queries, because one would have to
encode the semantics of SQL in the type system (or in a rule engine of some sort), and this is clearly not the kind of
things I want to spend my youth on.

Each function is tested for its output with doctests, and the ones that cannot (due to database connections) are tested
in the more traditional test-suite.

The conclusion is : Test your DB queries. Test the encoding/decoding. Make roundtrip tests for your data-structures.

## Acknowledgements 

I wish to thank

* Clément Delafargue, whose [anorm-pg-entity][anorm-pg-entity] library and its [initial port in Haskell][entity-blogpost-fretlink]
  are the spiritual parents of this library
* Koz Ross, for his piercing eyes and his immense patience
* Joe Kachmar, who enlightened me many times

[docs]: https://img.shields.io/badge/Tutorial%20and%20Guides-pg--entity-blueviolet
[docs-url]: https://hackage.haskell.org/package/pg-entity/src/docs/book/index.html
[docspec]: https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md
[pg-transact-hspec]: https://github.com/jfischoff/pg-transact-hspec.git
[entity-blogpost-fretlink]: https://tech.fretlink.com/yet-another-unsafe-db-layer/
[anorm-pg-entity]: https://github.com/CleverCloud/anorm-pg-entity
[pg-simple]: https://hackage.haskell.org/package/postgresql-simple
[pg-transact]: https://hackage.haskell.org/package/pg-transact
[text]: https://hackage.haskell.org/package/text
[vector]: https://hackage.haskell.org/package/vector
[CI-badge]: https://img.shields.io/github/workflow/status/tchoutri/pg-entity/CI?style=flat-square
[CI-url]: https://github.com/tchoutri/pg-entity/actions
[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square
[BlogPost-module]: https://github.com/tchoutri/pg-entity/blob/main/src/Database/PostgreSQL/Entity/Internal/BlogPost.hs

