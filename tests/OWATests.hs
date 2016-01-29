module Main where

--import OWALib
--import Test.Hspec
import System.Directory
import AppDirectoryTests

main :: IO ()
main = do
	currentFilePath <- getCurrentDirectory
	_ <- putStrLn currentFilePath
	runAppDirectoryTests currentFilePath
