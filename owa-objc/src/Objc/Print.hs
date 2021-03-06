{-|
Module      : Objc.Print
Description : Module for printing Objc file structures to files
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.Print (
  printStructureToFile
) where

import Data.Maybe (catMaybes)
import System.IO
import Text.PrettyPrint.Leijen as PPrint

import Objc.AbSyn
import Utils.OWAPrintUtil

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
sectionDoc (InterfaceSection typeName superclass possibleCategoryName protocols properties methods) = interfaceDoc typeName superclass possibleCategoryName protocols properties methods
sectionDoc (ImplementationSection typeName possibleCategoryName sections) = implementationDoc typeName possibleCategoryName sections
sectionDoc (MethodHeaderListSection maybeComment methods) = methodHeaderListSectionDoc maybeComment methods
sectionDoc (MethodImplementationListSection maybePragma methods) = methodImplementationListSectionDoc maybePragma methods
sectionDoc (LocalizedStringListSection name statements) = text "//" <+> text name PPrint.<$>
  vcat (map statementDoc statements) PPrint.<$> empty
sectionDoc CMainMethodSection = cMethodSectionDoc

commentDoc :: String -> Doc
commentDoc [] = text "//"
commentDoc str = text "//" <+> text str

includeDoc :: Import -> Doc
includeDoc (ModuleImport modName) = text "@import" <+> text modName <> semi
includeDoc (FileImport fileName) = text "#import \"" <> text fileName <> text "\""

forwardDeclDoc :: ForwardDeclaration -> Doc
forwardDeclDoc (TypedefDecl returnType' name paramTypes) = text "typedef" <+>
  typeDoc returnType' <+>
  parens (text $ '^':name) <>
  parens (hcat $ punctuate (text ", ") (map typeDoc paramTypes)) <>
  semi
forwardDeclDoc (EnumDecl enumName types) = indentBlock headerLine enumBody <> semi
  where headerLine = text "typedef NS_ENUM(NSInteger," <+> text enumName <> text ")"
        enumBody = vcat $ punctuate (text ",") (map text types)
forwardDeclDoc (ClassDecl className) = text "@class" <+> text className <> semi

interfaceDoc :: String -> Maybe String -> Maybe String -> [String] -> [ObjcProperty] -> [FileSection] -> Doc
interfaceDoc typeName superclass possibleCategoryName protocols properties sections = text "@interface" <+>
  text typeName <+> superDocOrParens PPrint.<$>
  middleSection PPrint.<$>
  endDoc
    where 
      propertySection = case properties of
        [] -> empty 
        _ -> vcatWithSpace (map propertyDoc properties) PPrint.<$> empty
      middleSection = case sections of
        [] -> propertySection
        _ -> propertySection PPrint.<$> vcat (map sectionDoc sections)
      protocolDocMay = if null protocols
        then Nothing
        else Just $ angles $ hcat (punctuate (text ", ") (map text protocols))
      superDocOrParens = case superclass of
        Just super -> hcat $ punctuate (text " ") (catMaybes [Just colon, Just (text super), protocolDocMay])
        Nothing -> case possibleCategoryName of
          Just name -> parens $ text name
          Nothing -> text "()"

implementationDoc :: String -> Maybe String -> [FileSection] -> Doc
implementationDoc typeName possibleCategoryName sections = text "@implementation" <+>
  nameDoc PPrint.<$>
  vcatWithSpace (map sectionDoc sections) PPrint.<$>
  endDoc
    where nameDoc = case possibleCategoryName of
                      Nothing -> text typeName
                      Just catName -> text typeName <+> parens (text catName)

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

propertyDoc :: ObjcProperty -> Doc
propertyDoc property = text "@property" <+>
  parens (hcat $ punctuate (text ", ") (map text $ propertyAttributes property)) <+>
  typeDoc (propertyType property) <+>
  text (propertyName property) <> semi

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
statementDoc (ForEachBlock decl varName statements) = indentBlock
  (text "for" <+> parens (expressionDoc decl <+> text "in" <+> expressionDoc varName))
  (vcat $ map statementDoc statements)
statementDoc (AssignStatement expr1 expr2) = expressionDoc expr1 <+>
  text "=" <+>
  expressionDoc expr2 <> semi
  
expressionDoc :: ObjcExpression -> Doc
expressionDoc SelfExpr = text "self"
expressionDoc (MethodCall callingExp (UserMethod method) args) = methodCallDoc
  callingExp (nameIntro method) (map paramTitle $ params method) args
expressionDoc (MethodCall callingExp libMethod args) = methodCallDoc
  callingExp (libNameIntro libMethod) (libParams libMethod) args
expressionDoc (CFunctionCall funcName exprs) = text funcName <>
  parens (hcat (punctuate (text ", ") (map expressionDoc exprs)))
expressionDoc (BinOp expr1 op expr2) = expressionDoc expr1 <+>
  opDoc op <+>
  expressionDoc expr2
expressionDoc (PropertyCall callingExp propName) = expressionDoc callingExp <>
  text "." <> text propName
expressionDoc (VoidBlock params' statements) = indentBlock 
  (text "^" <>
    parens (hcat $ punctuate (text ", ") (map blockParamDoc params')))
  (vcat $ map statementDoc statements)
expressionDoc (Var varName) = text varName
expressionDoc (VarDecl varType varName) = typeDoc varType <+> text varName
expressionDoc (DictionaryLit exprMappings) = text "@{" <> hcat (punctuate (text ", ") (map keyValueDoc exprMappings)) <> text "}"
expressionDoc (StringLit stringVal) = text "@\"" <> text stringVal <> text "\""
expressionDoc (CStringLit stringVal) = text "\"" <> text stringVal <> text "\""
expressionDoc (FloatLit floatVal) = text $ truncatedFloatString floatVal
expressionDoc (ArrayLit expressions) = text "@[" <>
  hcat (punctuate (text ", ") (map expressionDoc expressions)) <>
  text "]"
expressionDoc (BoolLit b) = text (if b then "YES" else "NO")

keyValueDoc :: (ObjcExpression, ObjcExpression) -> Doc
keyValueDoc (key, value) = expressionDoc key <+> colon <+> expressionDoc value

methodCallDoc :: ObjcExpression -> String -> [String] -> [ObjcExpression] -> Doc
methodCallDoc callingExp nameIntro' titles paramExps = brackets $
  expressionDoc callingExp <+>
  text nameIntro' <>
  hsep (zipWith argDoc titles paramExps)

argDoc :: String -> ObjcExpression -> Doc
argDoc title objcExp = text title <> colon <> expressionDoc objcExp

staticSignifier :: Bool -> Doc
staticSignifier isStatic' = if isStatic' then text "+" else text "-"

typeDoc :: ObjcType -> Doc
typeDoc (PointerType typeName) = text typeName <> text "*"
typeDoc (SimpleType typeName) = text typeName

opDoc :: Operator -> Doc
opDoc Assign = equals

pragmaDoc :: String -> Doc
pragmaDoc sectionName = text "#pragma mark -" <+> text sectionName

endDoc :: Doc
endDoc = text "@end" PPrint.<$> empty

-- Unless such a time comes as we'll want to add more C functionality, there's no sense
-- extending the Objective C abstract syntax to handle one C function. So just hardcode this.
cMethodSectionDoc :: Doc
cMethodSectionDoc = 
  text "int main(int argc, char* argv[]) {" PPrint.<$>
  text "  @autoreleasepool {" PPrint.<$>
  text "    return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));" PPrint.<$>
  text "  }" PPrint.<$>
  text "}" PPrint.<$>
  empty
