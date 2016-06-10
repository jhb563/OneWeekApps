module TestButtonObjects where

import OWAElements
import Text.Parsec.Pos
import OWAView

testImageButton :: OWAButton
testImageButton = OWAButton {
  buttonName = "myButton",
  buttonText = Nothing,
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Just "file1.png"
}

heightConstraint :: OWAConstraint
heightConstraint = OWAConstraint {
  firstElementName = "myButton",
  firstAttribute = Height,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 50
}

widthConstraint :: OWAConstraint
widthConstraint = OWAConstraint {
  firstElementName = "myButton",
  firstAttribute = Width,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 100
}

topConstraint :: OWAConstraint
topConstraint = OWAConstraint {
  firstElementName = "myButton",
  firstAttribute = Top,
  secondElementName = Just "Super",
  secondAttribute = Just Top,
  multiplier = 1.0,
  constant = 0
}

leftConstraint :: OWAConstraint
leftConstraint = OWAConstraint {
  firstElementName = "myButton",
  firstAttribute = LeftSide,
  secondElementName = Just "Super",
  secondAttribute = Just LeftSide,
  multiplier = 1.0,
  constant = 0
}

testSuccessView :: OWAView
testSuccessView = OWAView {
  viewName = "myView",
  viewType = "OWAImageButtonView",
  subviews = [ButtonElement testImageButton],
  constraints = [heightConstraint, widthConstraint,
                topConstraint, leftConstraint]
} 

missingQuotesError :: SourcePos
missingQuotesError = newPos "imageButtonError1.view" 4 16 

wrongTagNameError :: SourcePos
wrongTagNameError = newPos "imageButtonError2.view" 4 7 
