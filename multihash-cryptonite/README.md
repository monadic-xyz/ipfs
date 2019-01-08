Haskell implementation of [multihash](https://github.com/multiformats/multihash): "self-identifying hashes".

In contrast to other implementations, this library assumes that applications
mainly work with the `Crypto.Hash.Digest` type (from the [cryptonite](https://hackage.haskell.org/package/cryptonite) package),
and use multihash as a wire encoding, or treat an encoded multihash as an opaque
value (ie. destructuring into a product is not provided).
