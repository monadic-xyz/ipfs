A collection of Haskell libraries for interacting with IPFS_

................................................................................

.. image:: https://travis-ci.org/oscoin/ipfs.svg?branch=master
   :target: https://travis-ci.org/oscoin/ipfs

................................................................................

.. contents::
   :local:
   :backlinks: none
   :depth: 1

Overview
================================================================================

This repository provides a number of packages for interacting with IPFS_ via its
`HTTP API`_. We do **not** aim to provide a full IPFS implementation.

Most of the work has been driven by the immediate requirements of the radicle_
code collaboration platform. Thus, it should be considered a work-in-progress,
and the individual packages may not surface the same level of granularity as
their official Go or Javascript counterparts. Contributions are welcome!

Package Overview
--------------------------------------------------------------------------------

Higher-level:
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

* `git-remote-ipfs <git-remote-ipfs>`_

   A `git remote helper`_ which allows storage of git repositories on IPFS.
   Based on git-remote-ipld_, with some extra features.

* `ipfs-api <ipfs-api>`_

   A client for the IPFS `HTTP API`_, based on servant-client_.

Multiformats_ and related packages:
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

* `binary-varint <binary-varint>`_
* `ipld-cid <ipld-cid>`_
* `multibase <multibase>`_
* `multihash-cryptonite <multihash-cryptonite>`_
* `multihash-serialise <multihash-serialise>`_


Building
================================================================================

The canonical build for this project is using cabal_ (>= 2.4, "new-build). For
convencience, we also provide a ``stack.yaml``. stack_ >= 1.9.1 is required to
meet the version requirements for ``Cabal``.

Please see the package READMEs for additional instructions, specifically test
setups.

Contributing
================================================================================

We accept contributions via PRs_. Please use issues_ to find out about where
help is most appreciated, and to propose larger changes requiring some upfront
discussion.

License
================================================================================

`BSD-3-Clause <LICENSE>`_ © Monadic GmbH

.. _IPFS: https://ipfs.io
.. _HTTP API: https://docs.ipfs.io/reference/api/http/
.. _radicle: https://radicle.xyz
.. _cabal: https://www.haskell.org/cabal
.. _stack: https://www.haskellstack.org
.. _PRs: https://github.com/oscoin/ipfs/pulls
.. _issues: https://github.com/oscoin/ipfs/issues
.. _Multiformats: https://github.com/multiformats/multiformats
.. _git remote helper: https://git-scm.com/docs/git-remote-helpers
.. _git-remote-ipld: https://gitub.com/ipfs-shipyard/git-remote-ipld
.. _servant-client: https://hackage.haskell.org/package/servant-client
