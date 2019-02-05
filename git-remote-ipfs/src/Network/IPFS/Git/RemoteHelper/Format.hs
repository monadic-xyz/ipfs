module Network.IPFS.Git.RemoteHelper.Format
    ( fmt
    , sfmt
    , fstr
    , ftxt
    , fint
    , fcid
    , fref
    , frefName

    , (%)
    , shown
    )
where

import           Data.Text (Text)
import           Formatting

import qualified Data.Git.Named as Git (RefName, refNameRaw)
import qualified Data.Git.Ref as Git (Ref, toHexString)
import           Data.IPLD.CID (CID, cidToText)

fmt :: Format Text a -> a
fmt = sformat

sfmt :: Format String a -> a
sfmt = formatToString

fstr :: Format r (String -> r)
fstr = string

ftxt :: Format r (Text -> r)
ftxt = stext

fint :: Integral a => Format r (a -> r)
fint = int

fcid :: Format r (CID -> r)
fcid = mapf cidToText stext

fref :: Format r (Git.Ref hash -> r)
fref = mapf Git.toHexString string

frefName :: Format r (Git.RefName -> r)
frefName = mapf Git.refNameRaw string
