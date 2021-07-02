## pg-entity tutorial

In this tutorial, you will learn how to implement the Entity typeclass for your business logic data-types, and run
queries against the database.

### Setting up our data-types
First, let us enable a couple of extensions 

```haskell
-- Traditional list & string syntax
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- Quasi-quoter to construct SQL expressions
{-# LANGUAGE QuasiQuotes #-}
-- Deriving machinery
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
```

* `OverloadedLists` and `OverloadedStrings` allow us to use the `[list]` and `"string"`
syntaxes for datatypes other than Lists and Strings (in our case, Vector and Text).

* `QuasiQuotes` enable us to write plain SQL in a `[|quasi-quoter block|]`. 

* The Deriving extensions give us more powerful typeclass derivation. 


Then, let's import some data-types and modules:

```haskell
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Transact (DBT)

import Database.PostgreSQL.Entity
```

And let's write down our initial data models for a blog. `Author`, and `BlogPost`.  

```haskell
-- | It is good practice to wrap your primary key in a newtype to gain more
-- type-safety, but without losing access to typeclass instances of the
-- underlying type, with `deriving newtype`.
newtype AuthorId
  = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)

data Author
  = Author { -- | Primary key
             authorId  :: AuthorId
           , name      :: Text
           , createdAt :: UTCTime
           }
  deriving stock (Eq, Generic, Show) 
  -- ^ Instances that are provided by the Haskell Report are known as `stock` classes.
  deriving anyclass (FromRow, ToRow) 
  -- ^ Other instances are marked derived using the `anyclass` strategy
```

and

```haskell
newtype BlogPostId
  = BlogPostId { getBlogPostId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)

data BlogPost
  = BlogPost { -- | Primary key
               blogPostId :: BlogPostId
               -- | Foreign key
             , authorId   :: AuthorId
             , title      :: Text
             , content    :: Text
             , createdAt  :: UTCTime
             }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
```

Let us write the `Entity` instances now:

```Haskell
instance Entity Author where
  tableName  = "authors"
  primaryKey = "author_id"
  fields     = [ "author_id"
               , "name"
               , "created_at"
               ]
```

The above instance declaration reads as:  
> My table's name is _authors_, its primary key is _author\_id_, and the fields are _author\_id_, _name_, and _created\_at_.

The order matters for the declarations, so make sure that each field is at the correct position.

Let's do the same for `BlogPost`:

```Haskell
instance Entity BlogPost where
  tableName  = "blogposts"
  primaryKey = "blogpost_id"
  fields = [ "blogpost_id"
           , "author_id"
           , "title"
           , "content"
           , "created_at"
           ]
```

And these instances will give you access to the Entity functions to query your tables. 

### Using Generics

But all these manual instances are a tad tedious. Fortunately, there is a derivation mechanism available that allows you
to automate the generation of the `Entity` instance. This mechanism is called `DerivingVia`.
It allows you to use a wrapper, called `GenericEntity`, and a list of options at the type-level to generate the instance


```haskell
data Author
  = Author { authorId  :: AuthorId
           , name      :: Text
           , createdAt :: UTCTime
           }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[ TableName "authors"    
                        , PrimaryKey "author_id"
                        ] Author)
```

Those two options, `TableName` and `PrimaryKey` are optional, and the library will adopt the following defaults:

* `tableName` will be the snake\_case version of the record's name. No pluralisation will be done.
* `primaryKey` will be the snake\_case version of the first field of the record.
* `field` names will be the snake\_case version of the record fields.

The advantages brought by this technique are that it reduces the surface for errors when implementing the `Entity` typeclass,
and that the instance and the type are 

⚠ While all these deriving parameters are optional, I advise you to write down the table name (with the `TableName` option).  
In the PostgreSQL world, it is customary to pluralise the name of the table containing your data, and
`pg-entity` does not automatically do this.

<TODO> write the fact that if you declare the pk name, you don't do linear search in the fields list (O(n) -> O(1)).
Do it if you have long tables.

### Writing the SQL migrations

In a separate file, we will translate our Haskell data models into SQL migrations.

```sql
create table authors (
    author_id uuid primary key,
    name text not null,
    created_at timestamptz not null
    );

create table blogposts (
    blogpost_id uuid primary key,
    author_id uuid not null,
    title text not null,
    content text not null,
    created_at timestamptz not null,
    constraint fk_author
      foreign key(author_id)
        references authors(author_id)
    );
```

### Making queries

By implementing the `Entity` Typeclass, your data-type has access to a variety of functions, combinators and helpers that serve the one true purpose of
this library: 

> Provide a safe mechanism to expand the fields of a table while writing a query.

Let us define our insertion function for the Author model:

```haskell
insertAuthor :: Author -> DBT IO ()
insertAuthor = insert
```

<!--
TODO: Link to runDB in Hackage when the library is published
-->
The result of this function, which is called a “DBT action”, is then passed to `Database.PostgreSQL.Entity.DBT.runDB`

```haskell
runDB :: (MonadCatch m, MonadBaseControl IO m)
      => ConnectionPool         -- The connection pool,
                                -- ideally taken from an environment ReaderT
      -> DBT m a                -- The DB action, like `insert`
      -> m (Either DBError a)   -- The return value
```

You can then build a higher-level API endpoint or route controller like that:

```haskell
addAuthor :: ConnectionPool -> AuthorInfo -> IO ()
addAuthor pool info = do
  newAuthor <- mkAuthor info -- This functions is left to you
  result <- runDB pool $ insertAuthor newAuthor
  case result of
    Left err -> error "Could not insert author! Error " <> show err
    Right _ -> putTextLn "Author " <> (name newAuthor) <> " added!"
```

And if you want to later select an `Author` based on its `AuthorId`:

```haskell
getAuthor :: ConnectionPool -> AuthorId -> IO (Either DBError Author)
getAuthor pool authorId = runDB pool $ selectOneById (Only authorId)
```

This is the end of this tutorial. There are many more functions to discover that will help you write your queries.
While you shouldn't have to explore the source code to gain understanding of how to use the library, feel free to
navigate it to see how the underlying mechanisms work. :)
