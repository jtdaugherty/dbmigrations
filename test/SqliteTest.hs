module SqliteTest
    ( tests
    )
where

import Test.HUnit

import Database.Schema.Migrations.Backend.HDBC ()
import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

import Database.HDBC ( IConnection(..), catchSql, withTransaction )

tests :: (IConnection a) => a -> IO [Test]
tests conn = do
  let acts = [ isBootstrappedFalseTest
             , bootstrapTest
             , applyMigrationFailure
             , isBootstrappedTrueTest
             , revertMigrationFailure
             , revertMigrationNothing
             , revertMigrationJust
             ]
  sequence $ map (\f -> commit conn >> f conn) acts

bootstrapTest :: (IConnection a) => a -> IO Test
bootstrapTest conn = do
  bs <- getBootstrapMigration conn
  applyMigration conn bs
  tables <- getTables conn
  installed <- getMigrations conn
  return $ test $ [ "successfully bootstrapped" ~:
                    [mId bs] ~=? installed
                  , "installed_migrations table exists" ~:
                    ["installed_migrations"] ~=? tables ]

isBootstrappedTrueTest :: (IConnection a) => a -> IO Test
isBootstrappedTrueTest conn = do
  result <- isBootstrapped conn
  return $ test $ True ~=? result

isBootstrappedFalseTest :: (IConnection a) => a -> IO Test
isBootstrappedFalseTest conn = do
  result <- isBootstrapped conn
  return $ test $ False ~=? result

ignoreSqlExceptions :: IO a -> IO (Maybe a)
ignoreSqlExceptions act = (act >>= return . Just) `catchSql`
                       (\_ -> return Nothing)

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: (IConnection a) => a -> IO Test
applyMigrationFailure conn = do
    m1 <- newMigration "second"
    m2 <- newMigration "third"

    let m1' = m1 { mApply = "CREATE TABLE valid (a int)" }
    let m2' = m2 { mApply = "INVALID SQL" }

    -- Apply the migrations, ignore exceptions
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
                               applyMigration conn' m1'
                               applyMigration conn' m2'

    -- Check that none of the migrations were installed
    installed <- getMigrations conn
    return $ "successfully roll back failed apply" ~: ["root"] ~=? installed

revertMigrationFailure :: (IConnection a) => a -> IO Test
revertMigrationFailure conn = do
    m1 <- newMigration "second"
    m2 <- newMigration "third"

    let m1' = m1 { mApply = "CREATE TABLE valid (a int)"
                 , mRevert = Just "DROP TABLE valid"}
    let m2' = m2 { mApply = "SELECT * FROM valid"
                 , mRevert = Just "INVALID SQL"}

    withTransaction conn $ \conn' -> do
      applyMigration conn' m1'
      applyMigration conn' m2'

    installedBeforeRevert <- getMigrations conn

    -- Revert the migrations, ignore exceptions
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
      revertMigration conn' m2'
      revertMigration conn' m1'

    -- Check that none of the migrations were installed
    installed <- getMigrations conn
    return $ "successfully roll back failed revert" ~: installedBeforeRevert ~=? installed

revertMigrationNothing :: (IConnection a) => a -> IO Test
revertMigrationNothing conn = do
    m1 <- newMigration "second"

    let m1' = m1 { mApply = "SELECT 1"
                 , mRevert = Nothing }

    withTransaction conn $ \conn' -> do
      applyMigration conn' m1'

    installedAfterApply <- getMigrations conn
    assertBool "Check that the migration was applied" $ "second" `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    withTransaction conn $ \conn' -> do
      revertMigration conn' m1'

    installed <- getMigrations conn
    return $ test $ assertBool "Check that the migration was reverted" $ not $ "second" `elem` installed

revertMigrationJust :: (IConnection a) => a -> IO Test
revertMigrationJust conn = do
    m1 <- newMigration "second"

    let m1' = m1 { mApply = "CREATE TABLE temp (a int)"
                 , mRevert = Just "DROP TABLE temp" }

    withTransaction conn $ \conn' -> do
      applyMigration conn' m1'

    installedAfterApply <- getMigrations conn
    assertBool "Check that the migration was applied" $ "second" `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    withTransaction conn $ \conn' -> do
      revertMigration conn' m1'

    installed <- getMigrations conn
    return $ test $ assertBool "Check that the migration was reverted" $ not $ "second" `elem` installed
