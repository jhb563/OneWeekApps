module OWAObjcPrint (
  printStructureToFile
) where

import OWAObjcAbSyn
import Prelude hiding ((<$>))
import System.IO
import Text.PrettyPrint.Leijen as PrettyPrint

printStructureToFile ::  ObjcFile -> FilePath -> IO ()
printStructureToFile objcFile filePath = do
  let doc = docFromFile objcFile
  handle <- openFile filePath WriteMode
  hPutDoc handle doc
  hClose handle

docFromFile :: ObjcFile -> Doc
docFromFile (ObjcFile []) = empty
docFromFile (ObjcFile sections) = vcat (map sectionDoc sections)

sectionDoc :: FileSection -> Doc
sectionDoc (BlockCommentSection commentLines) = vcat (map commentDoc commentLines) <$> empty
sectionDoc (ImportsSection includes) = vcat (map includeDoc includes) <$> empty
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
  parens (text $ categoryName category) <$>
  vcatWithSpace (map methodHeaderDoc $ categoryMethods category) <$>
  endDoc

categoryImplementationDoc :: Category -> Doc
categoryImplementationDoc category = empty

methodHeaderDoc :: ObjcMethod -> Doc
methodHeaderDoc method = staticSignifier (isStatic method) <+>
  parens (typeDoc $ returnType method) <>
  text (nameIntro method) <>
  hcat (map headerArgDef $ params method) <>
  semi

staticSignifier :: Bool -> Doc
staticSignifier isStatic = if isStatic then text "+" else text "-"

typeDoc :: ObjcType -> Doc
typeDoc (PointerType typeName) = text typeName <> text "*"
typeDoc (SimpleType typeName) = text typeName

headerArgDef :: ParamDef -> Doc
headerArgDef paramDef = text (paramTitle paramDef) <>
  colon <>
  parens (typeDoc $ paramType paramDef) <>
  text (paramName paramDef)

vcatWithSpace :: [Doc] -> Doc
vcatWithSpace [] = empty
vcatWithSpace docs = empty <$> vcat docs <$> empty

endDoc :: Doc
endDoc = text "@end" <$> empty
