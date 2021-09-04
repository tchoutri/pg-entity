# Example of using MonadError to handle business errors

## Description

## Build

```bash
$ cabal build 
```

## Run

By default, the programme will try to connect to `postgres://postgres:postgres@localhost:5432/postgres`.
Please execute the file `migrations/add_e.sql` before executing the programme.

When the migration is done, you can run it like this:

```bash
$ cabal exec processing
```
