# PG-Entity [![CI-badge][CI-badge]][CI-url] [![docs][docs]][docs-url] ![simple-haskell][simple-haskell]

This library is a pleasant layer on top of [postgresql-simple][pg-simple]. 
It aims to be a convenient middle-ground between rigid ORMs and hand-rolled SQL query strings. Here is its philosophy:

* The serialisation/deserialisation part is left to the consumer, so you have to go with your own FromRow/ToRow instances.
  You are encouraged to adopt data types that model your business, rather than restrict yourself with the limits of what
  a SQL schema can represent. Use a intermediate data access object (DAO) that can easily be serialised and deserialised
  to and from a SQL schema, to and from which you will morph your business data-types.
* Escape hatches are provided at every level. The types that are manipulated are Query for which an `IsString` instance exists.
  Don't force yourself to use the higher-level API if the combinators work for you, and if those don't either, “Just Write SQL”™.

Its dependency footprint is optimised for my own setups, and as such it makes use of [text][text], [vector][vector],
[pg-transact][pg-transact] and [relude][relude].



Table of Contents
=================

* [Installation](#installation)
* [Usage](#usage)
* [Escape hatches](#escape-hatches)
* [Documentation](#documentation)
* [Acknowledgements](#acknowledgements)

## Installation

To use pg-entity in your project, add it to the `build-depends` stanza of you .cabal file,
and insert the following in your `cabal.project`:

```
 source-repository-package
     type: git
     location: https://github.com/tchoutri/pg-entity.git
     tag: <last commit in the GitHub Repo>
```

Don't forget to fill the `tag` with the commit you wish to pin.

Due to the fact that it depends on a non-Hackage version of [pg-transact-hspec][pg-transact-hspec],
Hackage upload will have to wait.

## Usage

This library aims to be a thin layer between that sits between rigid ORMs and hand-rolled SQL query strings.

Implement the `Entity` typeclass for your data-type, and parametrise the library functions  
with a [Type Application](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_applications.html): 

```Haskell
:set -XOverloadedLists
:set -XOverloadedStrings
:set -XQuasiQuotes

import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.SqlQQ

-- A straightforward table definition

newtype JobId = JobId { getJobId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)

data Job
  = Job { jobId     :: JobId
        , locked_at :: UTCTime
        , jobName   :: Text
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "jobs"] Job)

-- And a richer table definition that needs some type annotations

newtype BagId = BagId { getBagId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)

data Properties = P1 | P2 | P3

data Bag
  = Bag { bagId      :: BagId
        , someField  :: Vector UUID
        , properties :: Vector Properties
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)


instance Entity Bag where
  tableName  = "bags"
  primaryKey = "bag_id"
  fields     = [ "bag_id"
               , "some_field" `withType` "uuid[]"
               , "enums" `withType` "properties[]"
               ]

-- You can write specialised functions to remove the noise

insert :: Bag -> DBT IO ()
insert = insert @Bag

getById :: BagId -> DBT IO ()
getById bagId = selectById @Bag (Only bagId)

isJobLocked :: Int -> DBT IO (Only Bool)
isJobLocked jobId = queryOne Select q (Only jobId)
  where q = [sql| SELECT
                    CASE WHEN locked_at IS NULL then false
                         ELSE true
                     END
                   FROM jobs WHERE job_id = ?
            |]
```

For more examples, see the [BlogPost][BlogPost-module] module for the data-type that is used throughout the tests and doctests.

### Escape hatches

Safe SQL generation is a complex subject, and it is far from being the objective of this library. The main topic it
addresses is listing the fields of a table, which is definitely something easier. This is why every level of this wrapper
is fully exposed, so that you can drop down a level at your convience.

It is my personal belief, firmly rooted in experience, that we should not aim to produce statically-checked SQL and have
it "verified" by the compiler. The techniques that would allow that in Haskell are still far from being optimised and
ergonomic. As such, this library makes no effort to produce semantically valid SQL queries, because one would have to
encode the semantics of SQL in the type system (or in a rule engine of some sort), and this is clearly not the kind of
things I want to spend my youth on.

Now, that does not mean that we will blindly emit random strings. Each function is tested for its output with doctests,
and the ones that cannot (due to database connections) are tested in the more traditional test-suite.

The conclusion is : Test your DB queries. Test the encoding/decoding. Make roundtrip tests for your data-structures.

## Documentation

This library aims to be thoroughly tested, by the means of [docspecs](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md)
and more traditional tests for database roundtrips.

I aim to produce and maintain a decent documentation, therefore do not hesitate to raise an issue if you feel that
something is badly explained and should be improved.

You will find the Tutorial, How-to Guides and the reference [here][docs-url].

## Acknowledgements 

I wish to thank

* Clément Delafargue, whose [anorm-pg-entity][anorm-pg-entity] library and its [initial port in Haskell][blogpost]
  are the spiritual parents of this library
* Koz Ross, for his piercing eyes and his immense patience
* Joe Kachmar, who enlightened me many times

[docs]: https://img.shields.io/badge/docs-pg--entity-blueviolet?style=flat-square
[docs-url]: https://tchoutri.github.io/pg-entity/
[pg-transact-hspec]: https://github.com/jfischoff/pg-transact-hspec.git
[blogpost]: https://tech.fretlink.com/yet-another-unsafe-db-layer/
[anorm-pg-entity]: https://github.com/CleverCloud/anorm-pg-entity
[pg-simple]: https://hackage.haskell.org/package/postgresql-simple
[pg-transact]: https://hackage.haskell.org/package/pg-transact
[text]: https://hackage.haskell.org/package/text
[vector]: https://hackage.haskell.org/package/vector
[relude]: https://hackage.haskell.org/package/relude
[CI-badge]: https://img.shields.io/github/workflow/status/tchoutri/pg-entity/CI?style=flat-square
[CI-url]: https://github.com/tchoutri/pg-entity/actions
[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square
[BlogPost-module]: https://github.com/tchoutri/pg-entity/blob/main/src/Database/PostgreSQL/Entity/BlogPost.hs

