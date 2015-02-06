module Database.Schema.Migrations.Filesystem.Serialize
    ( serializeMigration
    )
where

import Data.Time () -- for UTCTime Show instance
import Data.Maybe ( catMaybes )
import Data.List ( intercalate )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )

type FieldSerializer = Migration -> Maybe String

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
      Just desc -> Just $ "Description: " ++ desc

serializeTimestamp :: FieldSerializer
serializeTimestamp m = Just $ "Created: " ++ (show $ mTimestamp m)

serializeDepends :: FieldSerializer
serializeDepends m = Just $ "Depends: " ++ (intercalate " " $ mDeps m)

serializeRevert :: FieldSerializer
serializeRevert m =
    case mRevert m of
      Nothing -> Nothing
      Just revert -> Just $ "Revert: |\n" ++
                     (serializeMultiline revert)

serializeApply :: FieldSerializer
serializeApply m = Just $ "Apply: |\n" ++ (serializeMultiline $ mApply m)

commonPrefix :: String -> String -> String
commonPrefix a b = map fst $ takeWhile (uncurry (==)) (zip a b)

commonPrefixLines :: [String] -> String
commonPrefixLines [] = ""
commonPrefixLines theLines = foldl1 commonPrefix theLines

serializeMultiline :: String -> String
serializeMultiline s =
    let sLines = lines s
        prefix = case commonPrefixLines sLines of
                   -- If the lines already have a common prefix that
                   -- begins with whitespace, no new prefix is
                   -- necessary.
                   (' ':_) -> ""
                   -- Otherwise, use a new prefix of two spaces.
                   _ -> "  "

    in unlines $ map (prefix ++) sLines

serializeMigration :: Migration -> String
serializeMigration m = intercalate "\n" fields
    where
      fields = catMaybes [ f m | f <- fieldSerializers ]
