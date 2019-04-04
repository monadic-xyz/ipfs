{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Network.IPFS.Git.RemoteHelper.Internal where

import qualified Crypto.Hash as C
import qualified Data.ByteString.BaseN as BaseN
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.Git.Ref as Git
import           Data.Git.Storage.Object (Object(..))
import           Data.Git.Types

import           Data.IPLD.CID
                 ( CID
                 , Codec(..)
                 , cidCodec
                 , cidFromText
                 , cidHash
                 , newCidV1
                 )
import           Data.Multihash (Multihashable)
import qualified Data.Multihash as Multihash


objectLinks :: Multihashable hash => Object hash -> Vector CID
objectLinks = \case
    ObjCommit   x -> commitLinks x
    ObjTag      x -> tagLinks    x
    ObjBlob     x -> blobLinks   x
    ObjTree     x -> treeLinks   x
    -- XXX: not sure, possibly useful in the future
    ObjDeltaOfs _ -> mempty
    ObjDeltaRef _ -> mempty

treeLinks :: forall hash. Multihashable hash => Tree hash -> Vector CID
treeLinks (Tree entries) = foldMap (Vector.singleton . cid) entries
  where
    cid (_,_,ref) = refToCid @hash ref

commitLinks :: Multihashable hash => Commit hash -> Vector CID
commitLinks Commit { commitTreeish, commitParents } =
       Vector.singleton (refToCid commitTreeish)
    <> Vector.fromList  (map refToCid commitParents)

tagLinks :: Multihashable hash => Tag hash -> Vector CID
tagLinks = Vector.singleton . refToCid . tagRef

blobLinks :: Blob hash -> Vector CID
blobLinks = const mempty

--------------------------------------------------------------------------------

cidFromHexShaText :: Text -> Either String CID
cidFromHexShaText t = do
    bytes  <- BaseN.decodeBase16Either $ Text.encodeUtf8 t
    digest <- note ("Invalid digest: " <> Text.unpack t) $ C.digestFromByteString @Git.SHA1 bytes
    pure $ newCidV1 GitRaw digest

hexShaFromCidText :: Text -> Either String Text
hexShaFromCidText t = do
    cid <- cidFromText t
    Text.pack . Git.toHexString <$> cidToRef @Git.SHA1 cid

refToCid :: forall hash. Multihashable hash => Git.Ref hash -> CID
refToCid =
    newCidV1 GitRaw
        . fromMaybe (error "refToCid: Subverted Git.Ref supplied")
        . C.digestFromByteString @hash
        . Git.toBinary

cidToRef :: Multihashable hash => CID -> Either String (Git.Ref hash)
cidToRef cid =
    case cidCodec cid of
        GitRaw ->
            fmap Git.fromDigest
                . Multihash.decodeDigest
                . Multihash.encodedBytes
                $ cidHash cid
        codec  -> Left $ "cidToRef: Unexpected codec `" <> show codec <> "`"

--------------------------------------------------------------------------------

note :: a -> Maybe b -> Either a b
note l = maybe (Left l) Right

hush :: Either a b -> Maybe b
hush = either (const Nothing) pure
