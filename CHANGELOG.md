# Revision history for Entity

## 0.0.2.0 -- YYYY-mm-dd

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

## 0.0.1.0 -- 2021-11-05

* First version. Released on an unsuspecting world.
