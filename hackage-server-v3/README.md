# hackage-server-v3

## Overview

`hackage-server-v3` is a successor to the excellent
[`hackage-server` version 2](https://github.com/haskell/hackage-server). V3 is a
bottom-up rewrite, which emphasizes a normalized data model in order to prepare
[hackage](https://hackage.haskell.org/) for the next several decades of its
growth.


## Building

`hackage-server-v3` can be built by a single invocation of
[stack](https://docs.haskellstack.org/en/stable/):

```
$ stack build
```


## Running

```
$ stack run hackage-server-v3 -- --help

Usage: hackage-server-v3 --db CONNECTION_STRING --blob-store FILE
                         [--num-connections INTEGER] [--port PORT]
                         --user-content-uri URI

  Hackage server v3

Available options:
  --db CONNECTION_STRING   PostgreSQL connection string
  --blob-store FILE        Path to a hackage server blob store. This directory
                           will be created if it doesn't already exist.
  --num-connections INT    The max number of connections to the database to keep
                           open.
  --port PORT              The port to serve hackage server on.
  --user-content-uri URI   The domain to serve user content from.
  -h,--help                Show this help text
```

In its present form, `hackage-server-v3` is intended to run as a reverse-proxy
in front of `hackage-server`. This means it can gracefully fall back to the
existing implementation for any routes it doesn't yet handle. This is an
intentional architectural decision, which allows us to gradually phase out
`hackage-server` v2 by prioritizing expensive routes to reimplement.

As a security precaution, `hackage-server-v3` serves user-uploaded content from
the domain given by `--user-content-uri`.


## Migrating from `hackage-server`

If you already have an existing `hackage-server` instance, you can import its
data into `hackage-server-v3` by way of the `hackage-server-v2-import` tool:

```
# Generate a database schema
stack run hackage-server-v2-import -- --db ${CONNECTION} make-db

# Backfill packages
stack run hackage-server-v2-import -- --db ${CONNECTION} backfill-packages ${DBPATH}

# Index tar blobs
stack run hackage-server-v2-import -- --db ${CONNECTION} backfill-blobstore ${DBPATH}/blobs
```

In the above, `${CONNECTION}` ought to be a postgresql connection string of the
form `postgresql://user:password@machine:port/database`. If you are running on
your local machine, `postgresql://user@/user` is often enough to get you up and
running.

`${DBPATH}` is a path to `hackage-server`'s `./state/db` folder.

*NOTE:* The steps given here are for a one-time backfill. There is not currently
any means for keeping the v2 and v3 databases synchronized in realtime.


## Technical Details

`hackage-server-v3`'s data model is organized around database rows, expressed as
[`rel8`](https://hackage.haskell.org/package/rel8)
[HKDs](https://reasonablypolymorphic.com/blog/higher-kinded-data/). We expose a
[`servant`](https://www.servant.dev/) API, and the server provides an
implementation of this API. We implement several custom servant combinators in
order to provide many `hackage-server` v2 specific quirks. HTML rendering is
performed statically via [`ede`](https://hackage.haskell.org/package/ede).

Rather notably, we consider *the Haskell code is the source of truth for the
database schema.* See
[`Rel8.CreateTable`](https://github.com/tweag/hackage-server-v3/blob/master/src/Rel8/CreateTable.hs)
whose `DbTable` type describes schemas, constraints and indices on tables.


## Performance

Conformance tests of v3 show initial results of ~3x speedup and 3% RAM usage
compared to v2.

These numbers have not yet been optimized for.


## Tests

In addition to the property tests you'd expect for hairy logic, we also provide:

- *template tests,* which show that `ede` templates exist, compile, and can
  successfully handle the data provided to them. These are automatically
  generated from the API schema.
- *endpoint tests,* which generate consistent models of a Hackage database, and
  test that the implemented routes satisfy properties of the model
- *conformance tests*, which show that v3's routes are byte-for-byte identical
  to `hackage-server`'s. These tests are currently rather hard to run, as they
  require having a v3-compatible subset of a v2 database. Unfortunately, the
  existing v2 export tools produce somewhat-corrupted copies of the original
  database.

