{-|
Module      : Swift.Print
Description : Module for printing Swift file structures to files
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.Print (
  printSwiftStructureToFile
) where

import System.IO
import Text.PrettyPrint.Leijen as PPrint

import Swift.AbSyn
import Utils.OWAPrintUtil

-------------------------------------------------------------------------------
---------------------------ENTRY METHOD----------------------------------------
-------------------------------------------------------------------------------

-- | Takes a Swift File Structure, converts it to a Doc, and
-- prints the doc out to the given file.
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
sectionDoc (ClassSection typeName supers sections) = classDoc typeName supers sections PPrint.<$> empty
sectionDoc (MethodImplementationListSection sectionTitle methods) = methodListSection sectionTitle methods
sectionDoc (StatementListSection sectionTitle statements) = statementListSection sectionTitle statements
sectionDoc (EnumSection enumName enumBaseType cases) = enumSection enumName enumBaseType cases PPrint.<$> empty
sectionDoc (ClassSpecifierSection specifier) = text "@" <> text specifier PPrint.<$> empty

commentDoc :: String -> Doc
commentDoc str = if null str
  then text "//"
  else text "//" <+> text str

importDoc :: Import -> Doc
importDoc (ModuleImport name') = text "import" <+> text name'

extensionDoc :: String -> [FileSection] -> Doc
extensionDoc extensionName sections = indentBlock 
  (text "extension" <+> text extensionName) 
  (vcatWithSpace $ map sectionDoc sections)

classDoc :: String -> [String] -> [FileSection] -> Doc
classDoc typeName supers sections = indentBlock
  (text "class" <+> text typeName <> colon <+> superDoc)
  (vcatWithSpace $ map sectionDoc sections)
  where
    superDoc = hcat $ punctuate (text ", ") (map text supers)

methodListSection :: Maybe String -> [SwiftMethod] -> Doc
methodListSection Nothing methods = spaceOut (map methodDoc methods)
methodListSection (Just title) methods = markDoc title PPrint.<$> empty PPrint.<$>
  spaceOut (map methodDoc methods)

statementListSection :: Maybe String -> [SwiftStatement] -> Doc
statementListSection Nothing statements = spaceOut (map statementDoc statements)
statementListSection (Just title) statements = markDoc title PPrint.<$> empty PPrint.<$>
  spaceOut (map statementDoc statements)

enumSection :: String -> SwiftType -> [String] -> Doc
enumSection name' typ cases = indentBlock headerDoc body
  where
    headerDoc = text "enum" <+> text name' <> colon <+> typeDoc typ
    body = vcat $ map (\ident -> text "case" <+> text ident) cases

methodDoc :: SwiftMethod -> Doc
methodDoc swiftMethod = indentBlock methodDef methodBodyDoc
  where 
    qualifierList = qualifiers swiftMethod ++
      if isInitializer swiftMethod then [] else ["func"]
    qualifierDoc = hsep (map text qualifierList)
    paramList = map paramDoc (params swiftMethod)
    returnDoc = case returnType swiftMethod of
                  Nothing -> empty
                  Just typ -> text " ->" <+>
                    typeDoc typ
    methodDef = qualifierDoc <+> 
                text (name swiftMethod) <> 
                parens (hcat $ punctuate (text ", ") paramList) <>
                returnDoc
    methodBodyDoc = vcat (map statementDoc $ methodBody swiftMethod)

typeDoc :: SwiftType -> Doc
typeDoc (SimpleType typ) = text typ
typeDoc (OptionalType typ) = typeDoc typ <> text "?"
typeDoc (ExplicitType typ) = text typ <> text "!"
typeDoc (FunctionType argTypes returnType') = parens 
  (hcat $ punctuate (text ", ") (map typeDoc argTypes)) <+>
  text "->" <+> typeDoc returnType'
typeDoc (DictionaryType keyType valueType) = brackets $
  typeDoc keyType <+> colon <+> typeDoc valueType

paramDoc :: ParamDef -> Doc
paramDoc pDef = case label of
  Nothing -> text "_" <+> titlePlusType
  Just l -> if l == title
    then titlePlusType
    else text l <+> titlePlusType
  where
    label = paramLabelName pDef
    title = paramTitle pDef
    titlePlusType = text title <> colon <+> typeDoc (paramType pDef)

--paramDoc paramDef = text (paramTitle paramDef) <> colon <+> typeDoc (paramType paramDef)

statementDoc :: SwiftStatement -> Doc
statementDoc (ReturnStatement expr) = text "return" <+> expressionDoc expr
statementDoc (ExpressionStatement expr) = expressionDoc expr
statementDoc (LetDecl name' expr) = text "let" <+> text name' <+>
  text "=" <+> expressionDoc expr
statementDoc (VarDecl declQualfiers name' varType maybeExpr) = case maybeExpr of
  Nothing -> declDoc
  Just expr -> declDoc <+> text "=" <+> expressionDoc expr
  where
    declDoc = hsep 
      (map text (declQualfiers ++ ["var"])) <+>
      text name' <> colon <+> typeDoc varType
statementDoc (TypeAliasDecl name' typ) = text "typealias" <+> text name' <+> text "=" <+>
  typeDoc typ
statementDoc (AssignStatement expr1 expr2) = expressionDoc expr1 <+> text "=" <+>
  expressionDoc expr2
statementDoc (ForEachBlock varExpr containerExpr statements) = indentBlock
  forHeader
  forBody
  where
    forHeader = text "for" <+> expressionDoc varExpr <+> text "in" <+>
      expressionDoc containerExpr
    forBody = vcat (map statementDoc statements)

expressionDoc :: SwiftExpression -> Doc
expressionDoc (MethodCall callerExp method paramExps) = case callerExp of
  Nothing -> fullMethodCall
  Just expr -> expressionDoc expr <> text "." <> fullMethodCall
  where 
    methodName = case method of
      LibMethod { libMethodName = lMN} -> lMN
      UserMethod SwiftMethod { name = n } -> n
    mParamNames = case method of
      UserMethod swiftMethod -> map paramLabelName (params swiftMethod)
      LibMethod {libParams = lParams} -> lParams
    zippedPairs = zip mParamNames (map expressionDoc paramExps) 
    pairedDocs = map (\(label, exprDoc) -> case label of
      Nothing -> exprDoc
      Just l -> text l <> colon <+> exprDoc) zippedPairs
    fullParamListDoc = hcat $ punctuate (text ", ") pairedDocs
    fullMethodCall = text methodName <> parens fullParamListDoc
expressionDoc (PropertyCall expr ident) = expressionDoc expr <> text "." <> text ident
expressionDoc (ClosureExpr closure) = nest 2 
  (headDoc PPrint.<$> 
    vcat (map statementDoc (closureBody closure)))
  PPrint.<$> text "}"
  where
    ps = closureParams closure
    headDoc = if null ps 
      then text "{"
      else text "{" <> parens (hcat $ punctuate (text ", ") (map paramDoc ps)) <+> text "in"
expressionDoc (CalledClosure closure params') = expressionDoc (ClosureExpr closure) <>
  parens (hcat $ punctuate (text ", ") (map expressionDoc params'))
expressionDoc (Var ident) = text ident
expressionDoc (FloatLit flt) = text $ truncatedFloatString flt
expressionDoc (StringLit str) = dquotes $ text str
expressionDoc (BoolLit b) = text $ if b then "true" else "false"
expressionDoc (ArrayLit exprs) = brackets $ hcat $ punctuate (text ", ")
  (map expressionDoc exprs)
expressionDoc (DictionaryLit []) = 
  (typeDoc $ DictionaryType (SimpleType "String") (SimpleType "AnyObject")) <> 
  text "()"
expressionDoc (DictionaryLit exprPairs) = brackets $ hcat $ punctuate (text ", ")
  (map exprPairToDoc exprPairs)
  where
    exprPairToDoc (keyExpr, valueExpr) = expressionDoc keyExpr <+> colon <+>
      expressionDoc valueExpr
expressionDoc (ExplicitExpr expr) = expressionDoc expr <> text "!"
expressionDoc (OptionalExpr expr) = expressionDoc expr <> text "?"

markDoc :: String -> Doc
markDoc title = text "// MARK:" <+> text title
