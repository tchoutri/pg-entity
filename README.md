# PG-Entity [![CI-badge][CI-badge]][CI-url] [![docs][docs]][docs-url] ![simple-haskell][simple-haskell]

This library is a pleasant layer on top of [postgresql-simple][pg-simple] to safely expand the fields of a table when
writing SQL queries.  
It aims to be a convenient middle-ground between rigid ORMs and hand-rolled SQL query strings. Here is its philosophy:

* The serialisation/deserialisation part is left to the consumer, so you have to go with your own FromRow/ToRow instances.
  You are encouraged to adopt data types that model your business, rather than restrict yourself within the limits of what
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
  * [Usage](#usage)
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
  - pg-entity-0.0.1.0
```

## Documentation

This library aims to be thoroughly tested, by the means of Oleg Grerus' [cabal-docspec][docspec]
and more traditional tests for database roundtrips.

I aim to produce and maintain a decent documentation, therefore do not hesitate to raise an issue if you feel that
something is badly explained and should be improved.

You will find the Tutorial [here][docs-url], and you will find below a short showcase of the library.

### Usage

The idea is to implement the `Entity` typeclass for the datatypes that represent your PostgreSQL table. 

```Haskell
-- Traditional list & string syntax
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- Quasi-quoter to construct SQL expressions
{-# LANGUAGE QuasiQuotes #-}
-- Deriving machinery
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.SqlQQ

import Database.PostgreSQL.Entity

-- This is our Primary Key newtype. It is wrapped in a newtype to make
-- it impossible to mitake with a plain `UUID`, but we still want to
-- benefit from the pre-existing typeclass instances that exist for
-- `UUID`. You can read the last two lines as:
-- > We use the definitions posessed by `UUID` for our own newtype.
newtype JobId = JobId { getJobId :: UUID }
  deriving (Eq, Show, FromField, ToField)
    via UUID

-- A straightforward table definition, which lets us use
-- the DerivingVia mechanism to declare the table name
-- in the `deriving` clause, and infer the fields and primary key.
-- The field names will be converted to snake_case.

data Job
  = Job { jobId    :: JobId
        , lockedAt :: UTCTime
        , jobName  :: Text
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving Entity
    via (GenericEntity '[TableName "jobs"] Job)

-- In the above deriving clause, we only had to specify the table name in order to pluralise it,
-- leaving the guessing of the primary key and the table names to the library.

-- Below is a richer table definition that needs some type annotations to help PostgreSQL.
-- We will have to write out the full instance by hand

newtype BagId = BagId { getBagId :: UUID }
  deriving (Eq, Show, FromField, ToField)
    via UUID

-- | This is a PostgreSQL Enum, which needs to be marked as such in SQL type annotations.
data Properties = P1 | P2 | P3
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

data Bag
  = Bag { bagId      :: BagId
        , someField  :: Vector UUID
        , properties :: Vector Properties
        }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)


instance Entity Bag where
  tableName  = "bags"
  primaryKey = [field| bag_id |]
  fields     = [ [field| bag_id |]
               , [field| some_field :: uuid[] |]
               , [field| properties :: properties[] |]
               ]

-- You can write specialised functions to remove the noise of Type Applications

insertBag :: Bag -> DBT IO ()
insertBag = insert -- `insert` will be specialised to `Bag`

-- And you can insert raw SQL through postgresql-simple

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

Each function is tested for its output with doctests, and the ones that cannot (due to database connections) are tested
in the more traditional test-suite.

The conclusion is : Test your DB queries. Test the encoding/decoding. Make roundtrip tests for your data-structures.

## Acknowledgements 

I wish to thank

* Clément Delafargue, whose [anorm-pg-entity][anorm-pg-entity] library and its [initial port in Haskell][entity-blogpost-fretlink]
  are the spiritual parents of this library
* Koz Ross, for his piercing eyes and his immense patience
* Joe Kachmar, who enlightened me many times

[docs]: https://img.shields.io/badge/docs-pg--entity-blueviolet?style=flat-square
[docs-url]: https://tchoutri.github.io/pg-entity/
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

