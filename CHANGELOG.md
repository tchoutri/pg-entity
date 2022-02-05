# Revision history for Entity

## 0.0.2.0 -- YYYY-mm-dd

* Add `queryOne_`, which takes no params and returns 0 or 1 results.
* Add `FieldModifiers` deriving option, which takes multiple modifiers:
  * `StripPrefix (prefix :: Symbol)`: You can remove a certain prefix from your field names
  * `CamelTo (separator :: Symbol)` and its variants, `CamelToSnake` and `CamelToKebab`: Transform field names written
  in CamelCase to snake\_case, kebab-case, or with a custom separator.
* Remove redundant metadata about the query nature when logging the query
* Add `selectOneWhereIn` that can match a row's column in a value of user-provided values

## 0.0.1.0 -- 2021-11-05

* First version. Released on an unsuspecting world.
