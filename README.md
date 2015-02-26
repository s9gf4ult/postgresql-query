# What ?

`postgresql-query` is a library for more simple query generation for
PostgreSQL database. It is not an ORM (but contains some part of). It
contains interpolating quasiquote for simple query generation.

# Motivation

When you want to perform some complex SQL query using
`postgresql-simple` you writing this query by hands like that:

```haskell
let q = mconcat
        [ "SELECT u.id, u.name, count(p.id) "
        , "FROM users as u INNER JOIN posts as p "
        , "ON u.id = p.user_id "
        , "WHERE u.name like ? AND u.created BETWEEN ? AND ? "
        , "GROUP BY u.id ORDER BY ? " ]
```

Well, this is not realy complex. Now what you need to perform the
query is to paste parameters instead of this `?` signs:

```haskell
query con q ("%Peyton%", now, yesterday)
```

Did you see the mistake? We forgot about ordering field. To perform this query we must

```haskell
query con q ("%Peyton%", now, yesterday, Identifier "...
```

Oups!. If we use `Identifier "u.name"` we will get `"u.name"` in our
query which is just not right. Sql syntax assumes `"u"."name"` for
this query. We can not use query parameter to paste optional field
here.


Next example is more complex:

```haskell
let name = Just "%Peyton%" -- assume we getting parameters from query
    minage = Just 10
    maxage = Just 50
    condlist = catMaybes
               [ const " u.name like ? " <$> name
               , const " u.age > ? " <$> minage
               , const " u.age < ? " <$> maxage ]
    paramlist = catMaybes
                [ toField <$> name
                , toField <$> minage
                , toField <$> maxage ]
    cond = if L.null condlist
           then mempty
           else "WHERE " <> (mconcat $ L.intersperse " AND " condlist)
    q = "SELECT u.id, u.name, u.age FROM users AS u " <> cond
query con q paramlist
```

So much to write and so many chances to make a mistake. What if we
could write this like:

```haskell
let name = Just "%Peyton%" -- assume we getting parameters from query
    minage = Just 10
    maxage = Just 50
    ord = "u.name" :: FN -- <- special type for field names!
    condlist = catMaybes
               [ (\a -> [sqlExp|u.name like #{a}|]) <$> name
               , (\a -> [sqlExp|u.age > #{a}|])     <$> minage
               , (\a -> [sqlExp|u.age < #{a}|])     <$> maxage ]
    cond = if L.null condlist
           then mempty
           else [sqlExp|WHERE ^{mconcat $ L.intersperse " AND " condlist}|]
pqQuery [sqlExp|SELECT u.id, u.name, u.age
                FROM users AS u ^{cond}
                ORDER BY ^{ord}|]
```

Much better!

# sqlExp

Quasiquote `sqlExp` has two way of interpolation:

* `#{exp}` - pastes inside query arbitrary value which type is
  instance of `ToField` typeclass. It performs correct strings
  escaping so it is not the same as stupid string interpolation. Dont
  worry about sql-injections when using it.

* `^{exp}` - pastes inside query arbitrary value which type has
  instance of `ToSqlBuilder` typeclass.

`sqlExp` returns `SqlBuilder` which has a `Monoid` instance
and made for effective concatination (bytestring builder works
inside).

This quasiquote correctly handles string literals and quoted
identifiers. It also removes line and block (even nested) sql comments
from resulting query as well as sequences of space characters. You are
free to write queries like

```sql
WHERE name SIMILAR TO '\^{2,3}' -- line comment #{ololo}
```

or even

```sql
WHERE "#{strange}identifier" SIMILAR TO '#{1,10}' /*nested/*^{block}*/comment*/
```

`sqlExp` will remove all comments and will not interpolate inside
string literals or quoted identifiers at all.

## sqlExpEmbed and sqlExpFile

If you have realy huge hardcore sql template you can

```haskell
pgQuery $(sqlExpEmbed "sql/foo/bar.sql")
```

It works just like Yesod's templates. You can use interpolation inside
templates like inside `sqlExp`.

```haskell
pgQuery $(sqlExpFile "foo/bar")
```

Is absolutely the same as above. It just prepends `sql/` and
appends `.sql` to your string. If you agree to follow naming
conventions you are welcome to use `sqlExpFile`.