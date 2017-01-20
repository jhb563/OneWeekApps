{-module AppInfoSearch where

import Core.FileSearch

appInfoSearchTests :: FilePath -> Spec
appInfoSearchTests currentDirectory = do
  let startPath1 = currentDirectory ++ appInfo1FolderExtension
  let startPath2 = currentDirectory ++ appInfo2FolderExtension
  let startPath3 = currentDirectory ++ appInfo3FolderExtension
  let startPath4 = currentDirectory ++ appInfo4FolderExtension
  describe "Find App Info File" $ do
    context "when in the immediate directory being searched" $
      it "Should return the app info file" $
        findAppInfoFile startPath1 `shouldReturn` (Just $ currentDirectory ++ appInfo1Extension)
    
    context "when one folder below the directory" $
      it "Should return the app info file" $
        findAppInfoFile startPath2 `shouldReturn` (Just $ currentDirectory ++ appInfo2Extension)

    context "when multiple folders below with a red herring directory" $
      it "Should return the app info file" $
        findAppInfoFile startPath3 `shouldReturn` (Just $ currentDirectory ++ appInfo3Extension)

    context "when there is no file" $
      it "Should return Nothing" $
        findAppInfoFile startPath4 `shouldReturn` Nothing


setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
  mapM_ (\fp -> createDirectoryIfMissing True $ currentDirectory ++ fp) directorySearchFolderExtensions
  mapM_ (createFileAndClose currentDirectory) directorySearchFileExtensions

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvFolderExtension)

testEnvFolderExtension :: FilePath
testEnvFolderExtension = "/testenv"

appInfo1FolderExtension :: FilePath
appInfo1FolderExtension = "/testenv/app1"

appInfo2FolderExtension :: FilePath
appInfo2FolderExtension = "/testenv/app2"

appInfo2FolderEndExtension :: FilePath
appInfo2FolderEndExtension = "/testenv/app2/src"

appInfo3FolderExtension :: FilePath
appInfo3FolderExtension = "/testenv/app3"

appInfo3FolderEndExtension :: FilePath
appInfo3FolderEndExtension = "/testenv/app3/Here/OneMore"

appInfo4FolderExtension :: FilePath
appInfo4FolderExtension = "/testenv/app4"

appInfo1Extension :: FilePath
appInfo1Extension = "/testenv/app1/app.info"

appInfo2Extension :: FilePath
appInfo2Extension = "/testenv/app2/src/app.info"

appInfo3Extension :: FilePath
appInfo3Extension = "/testenv/app3/Here/OneMore/app.info"

appInfo3RandomExtension :: FilePath
appInfo3RandomExtension  = "/testenv/app3/notHere"

appInfoFailExtension1 :: FilePath
appInfoFailExtension1 = "/testenv/app4/nope/level2"

appInfoFailExtension2 :: FilePath
appInfoFailExtension2 = "/testenv/app4/orThis/level3/level4"

directorySearchFolderExtensions :: [FilePath]
directorySearchFolderExtensions = [appInfo1FolderExtension,
  appInfo2FolderEndExtension,
  appInfo3FolderEndExtension,
  appInfo3RandomExtension,
  appInfoFailExtension1,
  appInfoFailExtension2]

directorySearchFileExtensions :: [FilePath]
directorySearchFileExtensions = [appInfo1Extension,
  appInfo2Extension,
  appInfo3Extension]

-- Setup Directory Structure
-- | testenv
-- -- | app1
-- -- -- | app.info
-- -- | app2
-- -- -- | src
-- -- -- -- | app.info
-- -- | app3
-- -- -- | notHere
-- -- -- | Here
-- -- -- -- | OneMore
-- -- -- -- -- app.info
-- -- | app4
-- -- -- | nope
-- -- -- -- | level2
-- -- -- | orThis
-- -- -- -- | level3
-- -- -- -- -- | level4 -}
