# Guides

## Error Handling

_Full code for this guide is located in the `example/` directory on GitHub_

Error handling is a tricky subject, and most often you will have to provide translation layers between your different components to express how a request
has failed and what is the relevant information to be reported.

For example, inserting twice the same entity with the same primary key will raise an error in the database engine that you have violated the uniqueness
constraint of a primary key. This is of little use for consumers of the system, who simply need to be told that their chosen email address or username is already used.

Building a top-down error datatype can be a very good or very bad idea, and should sometimes be replaced with a more extensible mechanism like Haskell
Exceptions (whose datatypes can be used outside of this mechanism, fortunately).

Let us consider a simple usecase, where we wish to express the following error modes:

* Entity was not found
* Entity is in a Bad State™
* Entity processing is running

```haskell
data EntityError
  = EntityNotFound
  | EntityBadState
  | EntityProcessingIsRunning
  deriving (Eq, Show)
```

A [`MonadError`][MonadError] stack can be used to handle errors with your data-type.

Here are the functions that we will be using:

```Haskell
-- | This function allows great control over the way we report errors,
-- and allows us to plug a `MonadError` for reporting.
withPool :: (MonadBaseControl IO m)
         => Pool Connection
         -> DBT m a
         -> m a
```

Those two functions show that we do not have to put a `MonadError` everywhere, and if a lower-level error happens, we can let it bubble up to create a
higher-level error (like status code 500 in an http server).
```Haskell
insertEntity :: (MonadIO m)
             => E
             -> DBT m ()

getEntity :: (MonadError EntityError m, MonadIO m)
          => Int
          -> DBT m E
getEntity key = do
  result <- selectById (Only key)
  case result of
    Just e -> pure e
    Nothing -> lift $ throwError EntityNotFound 
    -- ↑ Here, we convert a valid database response into a more precise
    -- business logic component.
```

## Arrays of items

Sometimes we want a field to be an array of items, like UUIDs. For such inner types, PostreSQL asks of you to
provide an explicit type signature. The case of arrays is interesting because you need to adapt to the syntax
wanted by PostreSQL: 

* An empty array is `{}`;
* An array with elements is `ARRAY[item1, item2, …, itemN]`
* An array with elements and a type signature is `ARRAY[item1, item2, …, itemN]::uuid[]`



[MonadError]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:MonadError

