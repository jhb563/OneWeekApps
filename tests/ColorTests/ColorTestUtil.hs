-- Combine items which are common to certain tests. This is sparse for the colors
-- example, but for future elements (fonts, views, etc.) we will want a similar
-- file to contain ALL common elements.

module ColorTestUtil (
  colorWithRGBAMethod
) where

import OWAObjcAbSyn

colorWithRGBAMethod :: ObjcMethod
colorWithRGBAMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "colorWith",
  returnType = PointerType "UIColor",
  params = 
    [ParamDef {
      paramTitle = "Red",
      paramType = SimpleType "CGFloat",
      paramName = "red"
    }, 
    ParamDef {
      paramTitle = "green",
      paramType = SimpleType "CGFloat",
      paramName = "green"
    }, 
    ParamDef {
      paramTitle = "blue",
      paramType = SimpleType "CGFloat",
      paramName = "blue"
    }, 
    ParamDef {
      paramTitle = "alpha",
      paramType = SimpleType "CGFloat",
      paramName = "alpha"
    }],
  methodBody = []
}
