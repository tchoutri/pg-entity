create table blogposts (
  blogpost_id uuid primary key,
  author_id uuid not null,
  title text not null,
  content text not null,
  created_at timestamptz not null
)
