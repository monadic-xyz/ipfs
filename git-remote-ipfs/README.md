Haskell implementation of a [git remote helper][0] to store git repositories on
[IPFS][1].

Reverse-engineered from the "official" [Go implementation][2], usable as
both an executable and a library. Future development is going to focus on the
latter.

# Install

1. Build the `git-remote-ipfs` executable using either

   ```
   stack install
   ```

   or

   ```
   cabal v2-install
   ```

   (requires `cabal-install` >= 2.4.0.0).

2. Make sure the executable is on your `$PATH`:

   ```
   export PATH=$HOME/.local/bin:$PATH
   ```

   or

   ```
   export PATH=$HOME/.cabal/bin:$PATH
   ```

3. [Download and install the `go-ipfs` binary][3], and make sure the ipfs daemon
   is running.

# Usage

To push a (branch of an) existing git repo to IPFS, using `.git/config` to
keep track of pushes:

```console
$ # Add a new remote
$ git remote add ipfs ipfs://
$ # Push master
$ git push ipfs master
$ # Inspect the IPFS path
$ git remote get-url ipfs
ipfs://ipfs/Qm....
```

Note that every push yields a new IPFS hash. The remote helper will rewrite the
remote URL locally to keep track of the latest remote refs. To collaborate with
other people (i.e. clone/pull from another machine), this URL needs to be
communicated out-of-band. The output of `git remote get-url` can be used to `git
clone`.

An alternative is to use [IPNS][4], which provides a stable name the remote
helper can update whenever the remote refs are updated:

```console
$ repoid=$(ipfs key gen --type=ed25519 myrepo)
$ git remote add ipns ipfs://ipns/$repoid
```

Note that resolving and updating an IPNS name is rather slow on the main IPFS
network. Also note that only the owner of the IPNS name (that is, the keypair)
can update it.

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

Which git objects need to be pushed or fetched is determined via the [git remote
helpers][0] protocol, respectively by inspecting the local git repo and remote
refs.

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
  conceptually, but terribly inefficient: regular git resorts to [packfiles][5],
  which use delta encoding and compression in order to obtain a more
  space-efficient on-disk and wire format. There is, however, no global optimum
  of how to pack any given git repo, and in fact git re-packs occasionally,
  as it sees fit. It is thus unclear how to optimise git storage in a fully
  distributed setting lacking online coordination.



[0]: https://git-scm.com/docs/git-remote-helpers
[1]: https://ipfs.io
[2]: https://github.com/ipfs-shipyard/git-remote-ipld
[3]: https://docs.ipfs.io/introduction/install/
[4]: https://docs.ipfs.io/guides/concepts/ipns/
[5]: https://git-scm.com/book/en/v2/Git-Internals-Packfiles
