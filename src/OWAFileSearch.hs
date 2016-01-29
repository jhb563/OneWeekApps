module OWAFileSearch (
	findAppDirectory
) where

import System.Directory

-- | 'findAppDirectory' Given a filepath, returns the filepath of the first directory it finds
-- whose name is 'app', via Breadth first search
findAppDirectory :: FilePath -> FilePath
findAppDirectory currentFilePath = currentFilePath