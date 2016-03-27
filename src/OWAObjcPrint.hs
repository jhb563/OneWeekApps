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
sectionDoc (ForwardDeclarationSection forwardDecls) = vcat (map forwardDeclDoc forwardDecls) PPrint.<$> empty
sectionDoc (CategoryInterfaceSection category sections) = categoryInterfaceDoc category sections
sectionDoc (CategoryImplementationSection category sections) = categoryImplementationDoc category sections
sectionDoc (MethodHeaderListSection maybeComment methods) = methodHeaderListSectionDoc maybeComment methods
sectionDoc (MethodImplementationListSection maybePragma methods) = methodImplementationListSectionDoc maybePragma methods
sectionDoc (LocalizedStringListSection name statements) = text "//" <+> text name PPrint.<$>
  vcat (map statementDoc statements) PPrint.<$> empty

commentDoc :: String -> Doc
commentDoc [] = text "//"
commentDoc str = text "//" <+> text str

includeDoc :: Import -> Doc
includeDoc (ModuleImport modName) = text "@import" <+> text modName <> semi
includeDoc (FileImport fileName) = text "#import \"" <> text fileName <> text "\""

forwardDeclDoc :: ForwardDeclaration -> Doc
forwardDeclDoc (TypedefDecl returnType name paramTypes) = text "typedef" <+>
  typeDoc returnType <+>
  parens (text $ '^':name) <>
  parens (hcat $ punctuate (text ", ") (map typeDoc paramTypes)) <>
  semi
forwardDeclDoc (EnumDecl enumName types) = indentBlock headerLine enumBody <> semi
  where headerLine = text "typedef NS_ENUM(NSInteger," <+> text enumName <> text ")"
        enumBody = vcat $ punctuate (text ",") (map text types)

categoryInterfaceDoc :: Category -> [FileSection] -> Doc
categoryInterfaceDoc category sections = text "@interface" <+> 
  text (originalTypeName category) <+> 
  parens (text $ categoryName category) PPrint.<$>
  vcatWithSpace (map sectionDoc sections) PPrint.<$>
  endDoc

categoryImplementationDoc :: Category -> [FileSection] -> Doc
categoryImplementationDoc category sections = text "@implementation" <+>
  text (originalTypeName category) <+>
  parens (text $ categoryName category) PPrint.<$>
  vcatWithSpace (map sectionDoc sections) PPrint.<$>
  endDoc

methodHeaderListSectionDoc :: Maybe String -> [ObjcMethod] -> Doc
methodHeaderListSectionDoc Nothing methods = vcat (map headerFileMethodHeaderDoc methods) PPrint.<$> empty
methodHeaderListSectionDoc (Just comment) methods = text "//" <+> text comment PPrint.<$>
  vcat (map headerFileMethodHeaderDoc methods) PPrint.<$>
  empty

methodImplementationListSectionDoc :: Maybe String -> [ObjcMethod] -> Doc
methodImplementationListSectionDoc Nothing methods = spaceOut (map fullMethodDoc methods)
methodImplementationListSectionDoc (Just pragma) methods = pragmaDoc pragma PPrint.<$>
  empty PPrint.<$>
  spaceOut (map fullMethodDoc methods)

headerFileMethodHeaderDoc :: ObjcMethod -> Doc
headerFileMethodHeaderDoc method = methodHeaderDoc method <> semi

methodHeaderDoc :: ObjcMethod -> Doc
methodHeaderDoc method = staticSignifier (isStatic method) <+>
  parens (typeDoc $ returnType method) <>
  text (nameIntro method) <>
  hcat (punctuate space (map headerArgDef $ params method))

fullMethodDoc :: ObjcMethod -> Doc
fullMethodDoc method = indentBlock (methodHeaderDoc method) body
                    where body = vcat (map statementDoc $ methodBody method)

headerArgDef :: ParamDef -> Doc
headerArgDef paramDef = text (paramTitle paramDef) <>
  colon <>
  parens (typeDoc $ paramType paramDef) <>
  text (paramName paramDef)

blockParamDoc :: BlockParam -> Doc
blockParamDoc param = typeDoc (blockParamType param) <+> text (blockParamName param)

statementDoc :: ObjcStatement -> Doc
statementDoc (ReturnStatement objcExpression) = text "return" <+>
  expressionDoc objcExpression <>
  semi
statementDoc (ExpressionStatement objcExpression) = expressionDoc objcExpression <> semi
statementDoc (IfBlock condition statements) = indentBlock
  (text "if" <+> parens (expressionDoc condition))
  (vcat $ map statementDoc statements)
  
expressionDoc :: ObjcExpression -> Doc
expressionDoc (MethodCall callingExp (UserMethod method) args) = methodCallDoc
  callingExp (nameIntro method) (map paramTitle $ params method) args
expressionDoc (MethodCall callingExp libMethod args) = methodCallDoc
  callingExp (libNameIntro libMethod) (libParams libMethod) args
expressionDoc (CFunctionCall funcName exprs) = text funcName <>
  parens (hcat (punctuate (text ", ") (map expressionDoc exprs)))
expressionDoc (BinOp expr1 op expr2) = expressionDoc expr1 <+>
  opDoc op <+>
  expressionDoc expr2
expressionDoc (VoidBlock params statements) = indentBlock 
  (text "^" <>
    parens (hcat $ punctuate (text ", ") (map blockParamDoc params)))
  (vcat $ map statementDoc statements)
expressionDoc (Var varName) = text varName
expressionDoc (VarDecl varType varName) = typeDoc varType <+> text varName
expressionDoc (DictionaryLit exprMappings) = text "@{" <> hcat (punctuate (text ", ") (map keyValueDoc exprMappings)) <> text "}"
expressionDoc (StringLit stringVal) = text "@\"" <> text stringVal <> text "\""
expressionDoc (CStringLit stringVal) = text "\"" <> text stringVal <> text "\""
expressionDoc (FloatLit floatVal) = text $ truncatedFloatString floatVal

keyValueDoc :: (ObjcExpression, ObjcExpression) -> Doc
keyValueDoc (key, value) = expressionDoc key <+> colon <+> expressionDoc value

methodCallDoc :: ObjcExpression -> String -> [String] -> [ObjcExpression] -> Doc
methodCallDoc callingExp nameIntro titles paramExps = brackets $
  expressionDoc callingExp <+>
  text nameIntro <>
  hsep (zipWith argDoc titles paramExps)

argDoc :: String -> ObjcExpression -> Doc
argDoc title objcExp = text title <> colon <> expressionDoc objcExp

staticSignifier :: Bool -> Doc
staticSignifier isStatic = if isStatic then text "+" else text "-"

typeDoc :: ObjcType -> Doc
typeDoc (PointerType typeName) = text typeName <> text "*"
typeDoc (SimpleType typeName) = text typeName

opDoc :: Operator -> Doc
opDoc Assign = equals

pragmaDoc :: String -> Doc
pragmaDoc sectionName = text "#pragma mark -" <+> text sectionName

-------------------------------------------------------------------------------
---------------------------Pretty Print Formatting Helpers --------------------
-------------------------------------------------------------------------------

indentBlock :: Doc -> Doc -> Doc
indentBlock doc1 doc2 = nest 2 (doc1 <+> text "{" PPrint.<$> doc2) PPrint.<$> text "}"

vcatWithSpace :: [Doc] -> Doc
vcatWithSpace [] = empty
vcatWithSpace docs = empty PPrint.<$> vcat docs

spaceOut :: [Doc] -> Doc
spaceOut [] = empty
spaceOut (headDoc:restDocs) = foldl (\d1 d2 -> d1 PPrint.<$> empty PPrint.<$> d2)
  headDoc restDocs PPrint.<$> empty

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
    _ -> initialString
  where initialString = Numeric.showFFloat (Just 3) flt ""
        decimalIndex = elemIndex '.' initialString
