{-|
Module      : OWAObjcPrint
Description : Module for printing Objc file structures to files
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAObjcPrint (
  printStructureToFile
) where

import Data.List
import Numeric
import OWAObjcAbSyn
import System.IO
import Text.PrettyPrint.Leijen as PPrint

-- | 'printStructureToFile' takes a file structure, and a file path, and
-- prints the file structure into the specified file using a PrettyPrint doc.
printStructureToFile ::  ObjcFile -> FilePath -> IO ()
printStructureToFile objcFile filePath = do
  let doc = docFromFile objcFile
  handle <- openFile filePath WriteMode
  hPutDoc handle doc
  hClose handle

-------------------------------------------------------------------------------
---------------------------Objective C Element Printers -----------------------
-------------------------------------------------------------------------------

docFromFile :: ObjcFile -> Doc
docFromFile (ObjcFile []) = empty
docFromFile (ObjcFile sections) = vcat (map sectionDoc sections)

sectionDoc :: FileSection -> Doc
sectionDoc (BlockCommentSection commentLines) = vcat (map commentDoc commentLines) PPrint.<$> empty
sectionDoc (ImportsSection includes) = vcat (map includeDoc includes) PPrint.<$> empty
sectionDoc (CategoryInterfaceSection category) = categoryInterfaceDoc category
sectionDoc (CategoryImplementationSection category) = categoryImplementationDoc category

commentDoc :: String -> Doc
commentDoc [] = text "//"
commentDoc str = text "//" <+> text str

includeDoc :: Import -> Doc
includeDoc (ModuleImport modName) = text "@import" <+> text modName <> semi
includeDoc (FileImport fileName) = text "#import \"" <> text fileName <> text "\""

categoryInterfaceDoc :: Category -> Doc
categoryInterfaceDoc category = text "@interface" <+> 
  text (originalTypeName category) <+> 
  parens (text $ categoryName category) PPrint.<$>
  vcatWithSpace (map headerFileMethodHeaderDoc $ categoryMethods category) PPrint.<$>
  endDoc

categoryImplementationDoc :: Category -> Doc
categoryImplementationDoc category = text "@implementation" <+>
  text (originalTypeName category) <+>
  parens (text $ categoryName category) PPrint.<$>
  spaceOut (map fullMethodDoc $ categoryMethods category) PPrint.<$>
  endDoc

headerFileMethodHeaderDoc :: ObjcMethod -> Doc
headerFileMethodHeaderDoc method = methodHeaderDoc method <> semi

methodHeaderDoc :: ObjcMethod -> Doc
methodHeaderDoc method = staticSignifier (isStatic method) <+>
  parens (typeDoc $ returnType method) <>
  text (nameIntro method) <>
  hcat (map headerArgDef $ params method)

fullMethodDoc :: ObjcMethod -> Doc
fullMethodDoc method = indentBlock (methodHeaderDoc method) body
                    where body = vcat (map statementDoc $ methodBody method)

headerArgDef :: ParamDef -> Doc
headerArgDef paramDef = text (paramTitle paramDef) <>
  colon <>
  parens (typeDoc $ paramType paramDef) <>
  text (paramName paramDef)

statementDoc :: ObjcStatement -> Doc
statementDoc (ReturnStatement objcExpression) = text "return" <+>
  expressionDoc objcExpression <>
  semi

expressionDoc :: ObjcExpression -> Doc
expressionDoc (MethodCall callingExp method args) = brackets $
  expressionDoc callingExp <+>
  text (nameIntro method) <>
  hsep (zipWith (curry argDoc) (params method) args)
expressionDoc (Var varName) = text varName
expressionDoc (FloatLit floatVal) = text $ truncatedFloatString floatVal

argDoc :: (ParamDef, ObjcExpression) -> Doc
argDoc (paramDef, objcExp) = text (paramTitle paramDef) <>
  colon <>
  expressionDoc objcExp

staticSignifier :: Bool -> Doc
staticSignifier isStatic = if isStatic then text "+" else text "-"

typeDoc :: ObjcType -> Doc
typeDoc (PointerType typeName) = text typeName <> text "*"
typeDoc (SimpleType typeName) = text typeName

-------------------------------------------------------------------------------
---------------------------Pretty Print Formatting Helpers --------------------
-------------------------------------------------------------------------------

indentBlock :: Doc -> Doc -> Doc
indentBlock doc1 doc2 = nest 2 (doc1 <+> text "{" PPrint.<$> doc2) PPrint.<$> text "}"

vcatWithSpace :: [Doc] -> Doc
vcatWithSpace [] = empty
vcatWithSpace docs = empty PPrint.<$> vcat docs PPrint.<$> empty

spaceOut :: [Doc] -> Doc
spaceOut [] = empty
spaceOut (headDoc:restDocs) = empty PPrint.<$> 
  foldl (\d1 d2 -> d1 PPrint.<$> empty PPrint.<$> d2) headDoc restDocs PPrint.<$> empty

endDoc :: Doc
endDoc = text "@end" PPrint.<$> empty

-------------------------------------------------------------------------------
---------------------------Float Formatting Helper ----------------------------
-------------------------------------------------------------------------------

truncatedFloatString :: Float -> String
truncatedFloatString flt = case decimalIndex of
  Nothing -> initialString
  Just index -> case reverse initialString of
    '0':'0':rest -> reverse rest
    '0':rest -> reverse rest
    otherwise -> initialString
  where initialString = Numeric.showFFloat (Just 3) flt ""
        decimalIndex = elemIndex '.' initialString
