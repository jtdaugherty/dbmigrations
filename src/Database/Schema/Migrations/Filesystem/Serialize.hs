module Database.Schema.Migrations.Filesystem.Serialize
    ( serializeMigration
    )
where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Text ( Text )
import qualified Data.Text as T
import Data.String.Conversions ( cs )
import Data.Time () -- for UTCTime Show instance
import Data.Maybe ( catMaybes )
import Data.Monoid ( (<>) )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )

type FieldSerializer = Migration -> Maybe ByteString

fieldSerializers :: [FieldSerializer]
fieldSerializers = [ serializeDesc
                   , serializeTimestamp
                   , serializeDepends
                   , serializeApply
                   , serializeRevert
                   ]

serializeDesc :: FieldSerializer
serializeDesc m =
    case mDesc m of
      Nothing -> Nothing
      Just desc -> Just . cs $ "Description: " <> desc

serializeTimestamp :: FieldSerializer
serializeTimestamp m =
    case mTimestamp m of
        Nothing -> Nothing
        Just ts -> Just $ "Created: " <> (cs . show $ ts)

serializeDepends :: FieldSerializer
serializeDepends m = Just . cs $ "Depends: " <> (T.intercalate " " $ mDeps m)

serializeRevert :: FieldSerializer
serializeRevert m =
    case mRevert m of
      Nothing -> Nothing
      Just revert -> Just $ "Revert: |\n" <>
                     (serializeMultiline revert)

serializeApply :: FieldSerializer
serializeApply m = Just $ "Apply: |\n" <> (serializeMultiline $ mApply m)

commonPrefix :: Text -> Text -> Text
commonPrefix a b = cs . map fst $ takeWhile (uncurry (==)) (T.zip a b)

commonPrefixLines :: [Text] -> Text
commonPrefixLines [] = ""
commonPrefixLines theLines = foldl1 commonPrefix theLines

serializeMultiline :: Text -> ByteString
serializeMultiline s =
    let sLines = T.lines s
        prefix = case T.head $ commonPrefixLines sLines of
                   -- If the lines already have a common prefix that
                   -- begins with whitespace, no new prefix is
                   -- necessary.
                   ' ' -> ""
                   -- Otherwise, use a new prefix of two spaces.
                   _ -> "  "

    in cs . T.unlines $ map (prefix <>) sLines

serializeMigration :: Migration -> ByteString
serializeMigration m = BS.intercalate "\n" fields
    where
      fields = catMaybes [ f m | f <- fieldSerializers ]
