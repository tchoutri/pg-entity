# Revision history for Entity

## 0.0.6.0 -- 2025-05-20

* The `prod` flag is removed, end of the experiment. End-users should be in charge of logging the queries.

## 0.0.5.1 -- 2025-04-23

* Update dependencies

## 0.0.5.0 -- 2024-01-19

* Update version of `bytestring` to 0.12, `text` to 2.1 and `postgresql-simple` to 0.7 ([#72](https://github.com/tchoutri/pg-entity/pull/72))

## 0.0.4.4 -- 2023-12-23
* Remove extraneous quoting of table name in _selectWithFields

## 0.0.4.3 -- 2023-06-26
* Fix the URL of the tutorial
* Bump version bounds for `base`, `resource-pool` and `template-haskell` 
* Bump supported minor GHC versions
* Internalise `Database.PostgreSQL.Entity.Internal.BlogPost` for the tests

## 0.0.3.0 -- 2022-10-30

* Fix compilation with resource-pool <0.3 (#51)

## 0.0.2.0 -- 2022-08-27

This is an experimental release

* Add `queryOne_`, which takes no params and returns 0 or 1 results.
* Add `FieldModifiers` deriving option, which takes multiple modifiers:
  * `StripPrefix (prefix :: Symbol)`: You can remove a certain prefix from your field names
  * `CamelTo (separator :: Symbol)` and its variants, `CamelToSnake` and `CamelToKebab`: Transform field names written
  in CamelCase to snake\_case, kebab-case, or with a custom separator.
* Remove redundant metadata about the query nature when logging the query
* Add `selectOneWhereIn` that can match a row's column in a value of user-provided values
* Add `selectOrderBy` to specify a vector of sorting specs (field + sorting keyword) with your select.
* Fix primary key detection in generic deriving
* Remove `withPool'`.
* When using `resource-pool-0.3`, the type of `withPool` reflects the removal of `MonadBaseControl` from the upstream.
* The `prod` Cabal flag is introduced. At this time, it disables the stdout logging of queries
* Add an `upsert` function
* Stop filtering out unknown fields passed to `_where`. PostgreSQL will report them better than we do.

## 0.0.1.0 -- 2021-11-05

* First version. Released on an unsuspecting world.
