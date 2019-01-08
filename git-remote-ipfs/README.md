Haskell implementation of a [git remote helper](https://git-scm.com/docs/git-remote-helpers) to store git repositories on [IPFS](https://ipfs.io).

Reverse-engineered from the "official" [Go implementation](https://github.com/ipfs-shipyard/git-remote-ipld/), usable as
both an executable and a library. Future development is going to focus on the
latter.

# Quick Start

* Build the executable using either

  ```
  stack install
  ```

  or

  ```
  cabal v2-install
  ```

  (requires `cabal-install` >= 2.4.0.0).

* Make sure the executable is on your `$PATH`.

* Add a new remote to your git repo:

  ```
  git remote add ipfs ipfs://
  ```

* Then push:

  ```
  git push ipfs master
  ```

Pushing will update the remote to point to an
`ipfs://` URL which links to the latest head(s). Use this URL to clone or pull
from another machine (`git remote -v` will show the current remote URL).

# How it works

IPFS blocks can be created with the `git-raw` CID format, which allows IPFS to
interpret the data as loose git objects. When created with `sha1` as the
(multihash) hash function, the block's CID corresponds to the SHA1 hash of the
git object, i.e. one can be recovered from the other. Crucially, this allows the
SHA1 references embedded in a loose git object (eg. parents and tree of a
commit) to be traversed given a head reference.

In order to obtain the head reference, IPLD links are created corresponding to
the `refs/heads` directory hierarchy. Note that adding links to an IPFS object
changes its hash - this means each push results in a new object (CID),
which must be retained in order to clone or pull.

Which git objects need to be pushed or fetched is determined via the [git
remote helpers](https://git-scm.com/docs/git-remote-helpers) protocol,
respectively by inspecting the local git repo and remote refs.

# Limitations

* It is currently unclear how to keep track of the latest "anchor" object (the
  one linking to the most recent heads). The obvious solution is to to use IPFS'
  native name resolution mechanism (IPNS), yet IPNS names have a very limited
  lifetime on the main IPFS network.

* IPFS blocks have a maximum size of 2MB. To work around this limitation,
  objects exceeding this limit are created as regular IPFS objects, linked back
  to the "anchor" object under the `objects/` hierarchy. When fetching, those
  large objects are given precedence over blocks, so as to not stall forever
  attempting to fetch blocks which the network does not replicate (_FIXME: this
  may not currently work as expected_).

* The approach to keep all git objects content-addressable in IPFS is nice
  conceptually, but terribly inefficient: regular git resorts to [packfiles](https://git-scm.com/book/en/v2/Git-Internals-Packfiles),
  which use delta encoding and compression in order to obtain a more
  space-efficient on-disk and wire format. There is, however, no global optimum
  of how to pack any given git repo, and in fact git re-packs occasionally,
  as it sees fit. It is thus unclear how to optimise git storage in a fully
  distributed setting lacking online coordination.
