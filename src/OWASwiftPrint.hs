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

import Data.List
import qualified Numeric
import OWASwiftAbSyn
import System.IO
import Text.PrettyPrint.Leijen as PPrint

printSwiftStructureToFile :: SwiftFile -> FilePath -> IO ()
printSwiftStructureToFile swiftFile filePath = do
  let doc = docFromFile swiftFile
  handle <- openFile filePath WriteMode
  hPutDoc handle doc
  hClose handle

-------------------------------------------------------------------------------
---------------------------SWIFT ELEMENT PRINTERS------------------------------
-------------------------------------------------------------------------------

docFromFile :: SwiftFile -> Doc
docFromFile (SwiftFile []) = empty
docFromFile (SwiftFile sections) = vcat (map sectionDoc sections)

sectionDoc :: FileSection -> Doc
sectionDoc (BlockCommentSection commentLines) = vcat (map commentDoc commentLines) PPrint.<$> empty
sectionDoc (ImportsSection imports) = vcat (map importDoc imports) PPrint.<$> empty
sectionDoc (ExtensionSection extensionName sections) = extensionDoc extensionName sections PPrint.<$> empty
sectionDoc (MethodImplementationListSection methods) = spaceOut (map methodDoc methods)

commentDoc :: String -> Doc
commentDoc str = if null str
  then text "//"
  else text "//" <+> text str

importDoc :: Import -> Doc
importDoc (ModuleImport name) = text "import" <+> text name

extensionDoc :: String -> [FileSection] -> Doc
extensionDoc extensionName sections = indentBlock 
  (text "extension" <+> text extensionName) 
  (vcatWithSpace $ map sectionDoc sections)

methodDoc :: SwiftMethod -> Doc
methodDoc swiftMethod = indentBlock methodDef methodBodyDoc
  where classDoc = if isClass swiftMethod then text "class " else empty
        paramList = map paramDoc (params swiftMethod)
        methodDef = classDoc <> 
                    text "func" <+> 
                    text (name swiftMethod) <> 
                    parens (hcat $ punctuate (text ", ") paramList) <+>
                    text "->" <+>
                    typeDoc (returnType swiftMethod)
        methodBodyDoc = vcat (map statementDoc $ methodBody swiftMethod)

typeDoc :: SwiftType -> Doc
typeDoc = text

paramDoc :: ParamDef -> Doc
paramDoc _ = empty

statementDoc :: SwiftStatement -> Doc
statementDoc (ReturnStatement expr) = text "return" <+> expressionDoc expr

expressionDoc :: SwiftExpression -> Doc
expressionDoc (MethodCall method paramExps) = text (libMethodName method) <>
  parens fullParamListDoc
  where mParamNames = case method of
          UserMethod swiftMethod -> map paramTitle (params swiftMethod)
          LibMethod {libParams = lParams} -> lParams
        zippedPairs = zip mParamNames (map expressionDoc paramExps) 
        pairedDocs = map (\(name, exprDoc) -> text name <> colon <> exprDoc) zippedPairs
        fullParamListDoc = hcat $ punctuate (text ", ") pairedDocs
expressionDoc (FloatLit flt) = text $ truncatedFloatString flt
        
-------------------------------------------------------------------------------
---------------------------Pretty Print Formatting Helpers --------------------
-------------------------------------------------------------------------------

indentBlock :: Doc -> Doc -> Doc
indentBlock doc1 doc2 = nest 2 (doc1 <+> text "{" PPrint.<$> doc2) PPrint.<$> text "}"

spaceOut :: [Doc] -> Doc
spaceOut [] = empty
spaceOut (headDoc:restDocs) = foldl (\d1 d2 -> d1 PPrint.<$> empty PPrint.<$> d2)
  headDoc restDocs PPrint.<$> empty

vcatWithSpace :: [Doc] -> Doc
vcatWithSpace [] = empty
vcatWithSpace docs = empty PPrint.<$> vcat docs

-------------------------------------------------------------------------------
---------------------------Float Formatting Helper ----------------------------
-------------------------------------------------------------------------------

truncatedFloatString :: Float -> String
truncatedFloatString flt = case decimalIndex of
  Nothing -> initialString
  Just index -> case reverse initialString of
    '0':'0':rest -> reverse rest
    '0':rest -> reverse rest
    _ -> initialString
  where initialString = Numeric.showFFloat (Just 3) flt ""
        decimalIndex = elemIndex '.' initialString
