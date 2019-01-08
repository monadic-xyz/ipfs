{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.Git.RemoteHelper.Command
    ( Command (..)
    , CommandResult (..)
    , OptRes (..)
    , ListRef (..)
    , PushRes (..)

    , parseCommand
    , renderCommandResult
    )
where

import           Control.Applicative (liftA2, optional, (<|>))
import           Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Char (isSpace)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)

import           Data.IPLD.CID (CID)

data Command
    = Capabilities
    | List
    | ListForPush
    | Option Text Text      -- name, value
    | Fetch Text Text       -- sha1, name
    | Push Bool Text Text   -- force, src ref, dest ref
    deriving Show

data CommandResult
    = CapabilitiesResult [Text]
    | ListResult         [ListRef]
    | ListForPushResult  [ListRef]
    | OptionResult       OptRes
    | FetchOk
    | PushResult         PushRes

data OptRes
    = OptionOk
    | OptionUnsupported
    | OptionErr Text

data ListRef = ListRef
    { listRefValue :: Maybe Text
    , listRefName  :: Text
    , listRefAttrs :: [Text]
    }

data PushRes = PushOk Text CID | PushErr Text Text

parseCommand :: P.Parser Command
parseCommand =
        (capabilities <?> "capabilities")
    <|> (listForPush  <?> "listForPush" )
    <|> (list         <?> "list"        )
    <|> (option       <?> "option"      )
    <|> (fetch        <?> "fetch"       )
    <|> (push         <?> "push"        )
  where
    capabilities = Capabilities <$ P.string "capabilities" <* eof
    list         = List <$ P.string "list" <* eof
    listForPush  = ListForPush <$ P.string "list for-push" <* eof

    option =
           P.string "option" *> P.skipSpace
        *> liftA2 Option (notSpace <* P.skipSpace) notSpace
        <* eof

    fetch =
           P.string "fetch" *> P.skipSpace
        *> liftA2 Fetch (notSpace <* P.skipSpace) notSpace
        <* eof

    push = do
        P.string "push" *> P.skipSpace
        force <- isJust <$> optional (P.char '+')
        src   <- decodeUtf8 <$> P.takeWhile1 (/= ':')  <* P.char ':'
        dst   <- decodeUtf8 <$> P.takeByteString
        pure $ Push force src dst

    notSpace = decodeUtf8 <$> P.takeWhile1 (not . isSpace)
    eof      = P.endOfInput

renderCommandResult :: CommandResult -> Text
renderCommandResult = \case
    CapabilitiesResult xs -> Text.unlines xs <> "\n"
    ListResult         xs -> Text.unlines (map renderListRef xs) <> "\n"
    ListForPushResult  xs -> Text.unlines (map renderListRef xs) <> "\n"
    OptionResult       x  -> renderOptRes x <> "\n"
    FetchOk               -> "\n"
    PushResult         x  -> renderPushRes x <> "\n\n"

renderListRef :: ListRef -> Text
renderListRef ListRef{..} =
       fromMaybe "?" listRefValue
    <> " "
    <> listRefName
    <> Text.intercalate " " listRefAttrs

renderOptRes :: OptRes -> Text
renderOptRes OptionOk          = "ok"
renderOptRes OptionUnsupported = "unsupported"
renderOptRes (OptionErr descr) = "error " <> descr

renderPushRes :: PushRes -> Text
renderPushRes (PushOk  dst _    ) = "ok " <> dst
renderPushRes (PushErr ref descr) = "error " <> ref <> " " <> descr
