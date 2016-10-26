{-|
Module      : OWASwiftUtil
Description : Utility module for common functions converting items to Swift Syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWASwiftUtil (
  extensionCommentSection,
  uiKitImportSection
) where

import Data.List.Split
import OWAAppInfo
import OWASwiftAbSyn

-------------------------------------------------------------------------------
-------------------BLOCK COMMENT FOR TOP OF FILE-------------------------------
-------------------------------------------------------------------------------

-- | Takes the file name and app info, and creates a file section for the
-- block comment at the top of all swift files.
extensionCommentSection :: String -> OWAAppInfo -> FileSection
extensionCommentSection filename appInfo = BlockCommentSection
  (definiteSection ++ possibleCompanySection)
  where dateCreated = dateCreatedString appInfo 
        createdString = "Created By " ++ authorName appInfo ++
          " " ++ dateCreatedString appInfo
        yearGiven = last $ splitOn "/" dateCreated
        yearCreated = if length yearGiven < 4 then "20" ++ yearGiven else yearGiven
        definiteSection = ["",
          filename,
          appName appInfo,
          "",
          createdString]
        possibleCompanySection = case companyName appInfo of
          Nothing -> [""]
          Just company -> ["Copyright (c) " ++ yearCreated ++ " " ++
            company ++ ". All Rights Reserved",
            ""]

-------------------------------------------------------------------------------
-------------------IMPORT SECTIONS---------------------------------------------
-------------------------------------------------------------------------------

-- | A simple file section importing UIKit
uiKitImportSection :: FileSection
uiKitImportSection = ImportsSection [ModuleImport "UIKit"]