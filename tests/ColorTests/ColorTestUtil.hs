-- Combine items which are common to certain tests. This is sparse for the colors
-- example, but for future elements (fonts, views, etc.) we will want a similar
-- file to contain ALL common elements.

module ColorTestUtil (
  colorWithRGBAMethod
) where

import OWAObjcAbSyn

colorWithRGBAMethod :: CalledMethod 
colorWithRGBAMethod = LibMethod {
  libNameIntro = "colorWith",
  libParams = ["Red", "green", "blue", "alpha"]
}
