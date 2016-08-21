{-|
Module      : OWASwiftPrint
Description : Module for printing Swift file structures to files
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWASwiftPrint (
  printSwiftStructureToFile
) where

import OWASwiftAbSyn
import System.IO
import Text.PrettyPrint.Leijen as PPrint

printSwiftStructureToFile :: SwiftFile -> FilePath -> IO ()
printSwiftStructureToFile swiftFile filePath = do
  handle <- openFile filePath WriteMode
  hPutDoc handle doc
  hClose handle

-------------------------------------------------------------------------------
---------------------------SWIFT ELEMENT PRINTERS------------------------------
-------------------------------------------------------------------------------

docFromFile :: SwiftFile -> Doc
docFromFile (SwiftFile []) = empty
docFromFile (SwiftFile sections) = empty
