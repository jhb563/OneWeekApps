{-|
Module      : OWAViewObjc
Description : Module for Converting OWAViews to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewObjc (
  objcHeaderFromView,
  objcImplementationFromView
) where

import ObjcUtil
import OWAAppInfo
import OWAObjcAbSyn
import OWAView

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromView' takes the app info and a view and returns
-- the structure for the view's header file.
objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcHeaderFromView appInfo view = ObjcFile 
  [topCommentSection (vTy ++ ".h") appInfo,
  uiKitImportsSection,
  InterfaceSection vTy (Just vanillaViewTypeKeyword) [] []]
    where vTy = viewType view

-- | 'objcImplementationFromView' takes the app info and a view and returns
-- the structure for the view's implementation file.
objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcImplementationFromView appInfo view = ObjcFile 
  [topCommentSection (vTy ++ ".m") appInfo,
  importsSection vTy (appPrefix appInfo),
  InterfaceSection vTy Nothing [] [],
  ImplementationSection vTy [lifecycleSection, setupSection view]]
    where vTy = viewType view

--------------------------------------------------------------------------------
--------------------------SECTION HELPERS---------------------------------------
--------------------------------------------------------------------------------

importsSection :: String -> String -> FileSection
importsSection viewType appPrefix = ImportsSection
  [FileImport (viewType ++ ".h"),
  FileImport colorFileName,
  FileImport fontFileName]
    where colorFileName = "UIColor+" ++ appPrefix ++ "Colors.h"
          fontFileName = "UIFont+" ++ appPrefix ++ "Fonts.h" 

lifecycleSection :: FileSection
lifecycleSection = MethodImplementationListSection (Just "Lifecycle") [initMethod]

setupSection :: OWAView -> FileSection
setupSection view = MethodImplementationListSection (Just "Setup") 
  [setupViewsMethod, setupConstraintsMethod]

--------------------------------------------------------------------------------
--------------------------METHOD HELPERS----------------------------------------
--------------------------------------------------------------------------------

initMethod :: ObjcMethod
initMethod = ObjcMethod {
  isStatic = False,
  nameIntro = "init",
  returnType = SimpleType "instancetype",
  params = [],
  methodBody = initBody
}
  where superCall = MethodCall 
                      (Var "super") 
                      LibMethod {libNameIntro = "init", libParams = []} 
                      []
        superStatement = ExpressionStatement $ BinOp (Var "self") Assign superCall
        ifBody = [ExpressionStatement $ MethodCall (Var "self") (UserMethod setupViewsMethod) [],
                  ExpressionStatement $ MethodCall (Var "self") (UserMethod setupConstraintsMethod) []]
        ifBlock = IfBlock (Var "self") ifBody
        returnStatement = ReturnStatement $ Var "self"
        initBody = [superStatement, ifBlock, returnStatement]

setupViewsMethod :: ObjcMethod
setupViewsMethod = ObjcMethod {
  isStatic = False,
  nameIntro = "setupViews",
  returnType = SimpleType "void",
  params = [],
  methodBody = setupViewsBody 
}
  where arrayDecl = ExpressionStatement $ BinOp
                      (VarDecl (PointerType "NSArray") "subviews")
                      Assign
                      (ArrayLit [])
        setMaskStatement = ExpressionStatement $ BinOp
                            (PropertyCall (Var "view") "translatesAutoresizingMaskIntoConstraints")
                            Assign
                            (Var "NO")
        addSubviewStatement = ExpressionStatement $ MethodCall (Var "self")
                                LibMethod {
                                  libNameIntro = "add",
                                  libParams = ["Subview"]
                                }
                                [Var "view"]
        forBlock = ForEachBlock
                    (VarDecl (PointerType "UIView") "view")
                    (Var "subviews")
                    [setMaskStatement, addSubviewStatement]
        setupViewsBody = [arrayDecl, forBlock]
  
setupConstraintsMethod :: ObjcMethod
setupConstraintsMethod = ObjcMethod {
  isStatic = False,
  nameIntro = "setupConstraints",
  returnType = SimpleType "void",
  params = [],
  methodBody = []
}
  
--------------------------------------------------------------------------------
--------------------------STRING CONSTANTS--------------------------------------
--------------------------------------------------------------------------------

vanillaViewTypeKeyword :: String
vanillaViewTypeKeyword = "UIView"
