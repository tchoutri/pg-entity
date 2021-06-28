## pg-entity tutorial

In this tutorial, you will learn the 

### Setting up our data-types
First, import the following modules:

```haskell
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Transact (DBT)
```

And let's write down our initial data models for a blog. `Author`, and `BlogPost`.  

```haskell
newtype AuthorId
  = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)

data Author
  = Author { authorId  :: AuthorId
           , name      :: Text
           , createdAt :: UTCTime
           }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
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

Thankfully, the instances for `FromRow` and `ToRow` were easy enough that they could be automatically derived.
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
> My table's name is _authors_, its primary key is _author\_id_, and the fields are _author_id_, _name_, and _created_at_.

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
to skip the manual parts that can be infered. This mechanism, called `DerivingVia`, allows you to use a wrapper, called
`GenericEntity`, and a list of options at the type-level, to specify the desired instance.

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
