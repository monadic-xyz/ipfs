{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Network.IPFS.API.Types (MultipartFormData) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L (ByteString)
import           Network.HTTP.Media ((//), (/:))
import           Servant.API

-- | Workaround for the lack of client support of \"servant-multipart\"
data MultipartFormData

instance Accept MultipartFormData where
    contentType _ = "multipart" // "form-data" /: ("boundary", boundary)

instance MimeRender MultipartFormData ByteString where
    mimeRender _ = render . Builder.byteString

instance MimeRender MultipartFormData L.ByteString where
    mimeRender _ = render . Builder.lazyByteString

instance MimeUnrender PlainText ByteString where
    mimeUnrender _ = Right . toStrict

boundary :: ByteString
boundary = "----------------------e46851f865c18c82"

render :: Builder -> L.ByteString
render bs = Builder.toLazyByteString $
       dash2 <> boundary' <> rn
    <> "Content-Disposition: form-data; name=\"files\"" <> rn <> rn
    <> bs <> rn
    <> dash2 <> boundary' <> dash2 <> rn
  where
    dash2     = Builder.byteString "--"
    rn        = Builder.byteString "\r\n"
    boundary' = Builder.byteString boundary
