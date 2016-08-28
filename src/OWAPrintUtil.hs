{-|
Module      : OWAPrintUtil
Description : Utility module for common functions for printing items in OWA
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAPrintUtil (
  indentBlock,
  spaceOut,
  vcatWithSpace,
  truncatedFloatString
) where

import Data.List
import Numeric
import Text.PrettyPrint.Leijen as PPrint

-------------------------------------------------------------------------------
---------------------------Pretty Print Formatting Helpers --------------------
-------------------------------------------------------------------------------

-- | 'indentBlock' creates a brace-delimited block which is indented by 2 spaces
-- It takes 2 docs, and makes new doc of the format:
-- doc1 {
--   doc2
-- }
indentBlock :: Doc -> Doc -> Doc
indentBlock doc1 doc2 = nest 2 (doc1 <+> text "{" PPrint.<$> doc2) PPrint.<$> text "}"

-- | 'spaceOut' takes a list of documents and concatenates them vertically,
-- but placing an empty line between each one. The returned Document does NOT
-- have a new line above or below. The "exception" is that is we get an empty
-- list we simply return the empty document.
spaceOut :: [Doc] -> Doc
spaceOut [] = empty
spaceOut (headDoc:restDocs) = foldl (\d1 d2 -> d1 PPrint.<$> empty PPrint.<$> d2)
  headDoc restDocs PPrint.<$> empty

-- | 'vcatWithSpace' is a version of vcat which places a space at the top
-- of the new documents. It handles the special case of no documents by
-- simply returning 1 empty line (instead of 2 empty lines, as would be
-- the case if empty list was not handled differently.
vcatWithSpace :: [Doc] -> Doc
vcatWithSpace [] = empty
vcatWithSpace docs = empty PPrint.<$> vcat docs

-------------------------------------------------------------------------------
---------------------------Float Formatting Helper ----------------------------
-------------------------------------------------------------------------------

-- | 'truncatedFloatString' takes a floating point number, truncates it to
-- 3 decimal places, and removes trailing 0's
truncatedFloatString :: Float -> String
truncatedFloatString flt = case decimalIndex of
  Nothing -> initialString
  Just index -> case reverse initialString of
    '0':'0':rest -> reverse rest
    '0':rest -> reverse rest
    _ -> initialString
  where initialString = showFFloat (Just 3) flt ""
        decimalIndex = elemIndex '.' initialString
