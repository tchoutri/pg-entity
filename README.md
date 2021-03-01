# PG-Entity ![CI][CI]

This library is a pleasant layer on top of [postgresql-simple][pg-simple]. It exposes both a high-level API and its 
building blocks, as to provide an escape hatch should the higher-level API fail to satisfy your needs.

Its dependency footprint is optimised for my own setups, and as such it makes use of [text][text], [vector][vector],
[pg-transact][pg-transact] and [relude][relude].

## Installation

To use pg-entity in your project, add it to your `build-depends` stanza and insert the following in your `cabal.project`:

```
 source-repository-package
     type: git
     location: https://github.com/tchoutri/pg-entity.git
     tag: <last commit in the GitHub Repo>
```

Don't forget to fill the `tag` with the desired commit you desire to pin.

Due to the fact that it depends on a non-Hackage version of [pg-transact-hspec][pg-transact-hspec], I doubt that I
will upload it on Hackage one day. We shall see.

## Documentation

Even though this work is mainly for my personal consumption, I encourage you to dive in the code and maybe get the 
necessary inspiration to build something greater. Therefore, I aim to maintain a decent documentation for this library.
Do not hesitate to raise an issue if you feel that something is badly explained and should be improved.

## TODO

* [ ] Documentation, doctests and `@since` tags
  * [ ] Entity.hs
  * [ ] DBT.hs
  * [ ] DBT.Types.hs
* [ ] Have a working CI

## Acknowledgements 

I wish to thank

* Clément Delafargue, whose [anorm-pg-entity][anorm-pg-entity] library and its [initial port in Haskell][blogpost]
  are the spiritual parents of this library
* Koz Ross, for his piercing eyes and his immense patience
* Joe Kachmar, who enlightened me many times

[pg-transact-hspec]: https://github.com/jfischoff/pg-transact-hspec.git
[blogpost]: https://tech.fretlink.com/yet-another-unsafe-db-layer/
[anorm-pg-entity]: https://github.com/CleverCloud/anorm-pg-entity
[pg-simple]: https://hackage.haskell.org/package/postgresql-simple
[pg-transact]: https://hackage.haskell.org/package/pg-transact
[text]: https://hackage.haskell.org/package/text
[vector]: https://hackage.haskell.org/package/vector
[relude]: https://hackage.haskell.org/package/relude
[CI]: https://github.com/tchoutri/pg-entity/workflows/CI/badge.svg?branch=main
