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
import OWAPrintUtil
import OWASwiftAbSyn
import System.IO
import Text.PrettyPrint.Leijen as PPrint

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
sectionDoc (ClassSection typeName superclassName sections) = classDoc typeName superclassName sections PPrint.<$> empty
sectionDoc (MethodImplementationListSection sectionTitle methods) = methodListSection sectionTitle methods
sectionDoc (StatementListSection sectionTitle statements) = statementListSection sectionTitle statements

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

classDoc :: String -> String -> [FileSection] -> Doc
classDoc typeName superclassName sections = indentBlock
  (text "class" <+> text typeName <> colon <+> text superclassName)
  (vcatWithSpace $ map sectionDoc sections)

methodListSection :: Maybe String -> [SwiftMethod] -> Doc
methodListSection Nothing methods = spaceOut (map methodDoc methods)
methodListSection (Just title) methods = markDoc title PPrint.<$> empty PPrint.<$>
  spaceOut (map methodDoc methods)

statementListSection :: Maybe String -> [SwiftStatement] -> Doc
statementListSection Nothing statements = spaceOut (map statementDoc statements)
statementListSection (Just title) statements = markDoc title PPrint.<$> empty PPrint.<$>
  spaceOut (map statementDoc statements)

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
typeDoc (OptionalType typ) = text typ <> text "?"
typeDoc (ExplicitType typ) = text typ <> text "!"

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
statementDoc (LetDecl name expr) = text "let" <+> text name <+>
  text "=" <+> expressionDoc expr
statementDoc (VarDecl declQualfiers name varType expr) = hsep 
  (map text (declQualfiers ++ ["var"])) <+>
  text name <> colon <+> typeDoc varType <+> text "=" <> expressionDoc expr
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
expressionDoc (Closure statements) = indentBlock empty (vcat (map statementDoc statements)) <> text "()"
expressionDoc (Var ident) = text ident
expressionDoc (FloatLit flt) = text $ truncatedFloatString flt
expressionDoc (StringLit str) = dquotes $ text str
expressionDoc (BoolLit bool) = text $ if bool then "true" else "false"
expressionDoc (ArrayLit exprs) = brackets $ hcat $ punctuate (text ", ")
  (map expressionDoc exprs)
expressionDoc (DictionaryLit exprPairs) = brackets $ hcat $ punctuate (text ", ")
  (map exprPairToDoc exprPairs)
  where
    exprPairToDoc (keyExpr, valueExpr) = expressionDoc keyExpr <+> colon <+>
      expressionDoc valueExpr

markDoc :: String -> Doc
markDoc title = text "// MARK:" <+> text title
