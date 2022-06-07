create table authors (
  author_id uuid primary key,
  name text not null,
  created_at timestamptz not null
);

create table blogposts (
  blogpost_id uuid primary key,
  author_id uuid not null,
  uuid_list uuid[] not null,
  title text not null,
  content text not null,
  created_at timestamptz not null,
  constraint fk_author
    foreign key(author_id)
      references authors(author_id)
);

create table faulty_entity (
  field1 uuid[] primary key,
  field2 uuid[]
);
