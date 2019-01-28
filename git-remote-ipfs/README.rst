Haskell implementation of a `git remote helper`_ to store git repositories on
IPFS_.

Reverse-engineered from the `Go implementation`_, usable as both an executable
and a library. Future development is going to focus on the latter.

.. _git remote helper: https://git-scm.com/docs/git-remote-helpers
.. _IPFS: https://ipfs.io
.. _Go implementation: https://github.com/ipfs-shipyard/git-remote-ipld

.. contents::
   :local:
   :backlinks: none

Install
================================================================================

1. Build the ``git-remote-ipfs`` executable using either ``stack install`` or
   ``cabal v2-install`` (requires ``cabal-install`` >= 2.4.0.0).

2. Make sure the executable is on your ``$PATH``:
   ``export PATH=$HOME/.local/bin:$PATH``, respectively
   ``export PATH=$HOME/.cabal/bin:$PATH``

3. Download and install the go-ipfs_ binary, and make sure the ipfs daemon is
   running.

.. _go-ipfs: https://docs.ipfs.io/introduction/install/

Usage
================================================================================

To push a (branch of an) existing git repo to IPFS, using `.git/config` to
keep track of pushes:

.. code:: shell

    $ # Add a new remote
    $ git remote add ipfs ipfs://
    $ # Push master
    $ git push ipfs master
    $ # Inspect the IPFS path
    $ git remote get-url ipfs
    ipfs://ipfs/Qm....

Note that every push yields a new IPFS hash. The remote helper will rewrite the
remote URL locally to keep track of the latest remote refs. To collaborate with
other people (i.e. clone/pull from another machine), this URL needs to be
communicated out-of-band. The output of `git remote get-url` can be used to `git
clone`.

An alternative is to use IPNS_, which provides a stable name the remote helper
can update whenever the remote refs are updated:

.. _IPNS: https://docs.ipfs.io/guides/concepts/ipns/

.. code:: shell

    $ repoid=$(ipfs key gen --type=ed25519 myrepo)
    $ # Publish a pointer to the empty directory initially
    $ ipfs name publish --key $repoid QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn
    $ git remote add ipns ipfs://ipns/$repoid

Note that resolving and updating an IPNS name is rather slow on the main IPFS
network. Also note that only the owner of the IPNS name (that is, the keypair)
can update it.

Configuration
--------------------------------------------------------------------------------

A number of options regarding IPFS can be configured via git-config_:

* ``ipfs.apiurl``

  The URL of the IPFS daemon, default: ``http://localhost:5001``

* ``ipfs.maxconnections``

  The max number of connections to open to the IPFS daemon, default: ``30``

* ``ipfs.maxblocksize``

  The maximum block size (in bytes) supported by bitswap. Most users will
  **not** want to change this. Default: ``2048000``

The API URL can be overriden per-remote using the key
``remote.<remote name>.ipfsapiurl``, e.g.:

.. code:: shell

    $ git config remote.origin.ipfsapiurl http://127.0.0.2:5001

Additionally, if the environment variable ``IPFS_API_URL`` is set, it will be
used instead of any git-config_ settings.

.. _git-config: https://git-scm.com/docs/git-config

How it works
================================================================================

IPFS blocks can be created with the ``git-raw`` CID format, which allows IPFS to
interpret the data as loose git objects. When created with ``sha1`` as the
(multihash) hash function, the block's CID corresponds to the SHA1 hash of the
git object, i.e. one can be recovered from the other. Crucially, this allows the
SHA1 references embedded in a loose git object (eg. parents and tree of a
commit) to be traversed given a head reference.

In order to obtain the head reference, IPLD links are created corresponding to
the ``refs/heads`` directory hierarchy. Note that adding links to an IPFS object
changes its hash - this means each push results in a new object (CID),
which must be retained in order to clone or pull.

Which git objects need to be pushed or fetched is determined via the `git remote
helper`_ protocol, respectively by inspecting the local git repo and remote
refs.

Limitations
--------------------------------------------------------------------------------

* It is currently unclear how to keep track of the latest "anchor" object (the
  one linking to the most recent heads). The obvious solution is to to use IPFS'
  native name resolution mechanism (IPNS), yet IPNS names have a `very limited
  lifetime <https://discuss.ipfs.io/t/ipns-max-lifetime/2130>`_ on the main IPFS
  network.

* IPFS blocks have a maximum size of 2MB. To work around this limitation,
  objects exceeding this limit are created as regular IPFS objects, linked back
  to the "anchor" object under the ``objects/`` hierarchy. When fetching, those
  large objects are given precedence over blocks, so as to not stall forever
  attempting to fetch blocks which the network does not replicate.

* The approach to keep all git objects content-addressable in IPFS is nice
  conceptually, but terribly inefficient: regular git resorts to packfiles_,
  which use delta encoding and compression in order to obtain a more
  space-efficient on-disk and wire format. There is, however, no global optimum
  of how to pack any given git repo, and in fact git re-packs occasionally, as
  it sees fit. It is thus unclear how to optimise git storage in a fully
  distributed setting lacking online coordination.

.. _packfiles: https://git-scm.com/book/en/v2/Git-Internals-Packfiles

Testing
================================================================================

The project employs an end-to-end test suite, which is disabled by default as it
requires a running IPFS daemon. The preferred way to run it is against a local
IPFS network, as this speeds up IPNS resolution considerably.

.. code:: shell

   $ docker run --detach --rm --name=ipfs-test-network --publish 19301:5001 \
      gcr.io/opensourcecoin/ipfs-test-network
   $ IPFS_API_URL=http://127.0.0.1:19301 \
      stack test --flag git-remote-ipfs:with-e2e-tests git-remote-ipfs
