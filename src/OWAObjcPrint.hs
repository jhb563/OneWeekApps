module OWAObjcPrint (
  printStructureToFile
) where

import OWAObjcAbSyn
import System.IO

printStructureToFile ::  ObjcFile -> FilePath -> IO ()
printStructureToFile objcFile filePath = do
  handle <- openFile filePath WriteMode
  hClose handle
