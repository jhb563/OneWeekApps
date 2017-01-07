module TestButtonObjects where

import Text.Parsec.Pos

import OWAElements
import OWAView

testImageButton :: OWAButton
testImageButton = OWAButton {
  buttonName = "button1",
  buttonText = Nothing,
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Just "file1.png"
}

heightConstraint :: OWAConstraint
heightConstraint = OWAConstraint {
  firstElementName = "button1",
  firstAttribute = Height,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 50
}

widthConstraint :: OWAConstraint
widthConstraint = OWAConstraint {
  firstElementName = "button1",
  firstAttribute = Width,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 100
}

topConstraint :: OWAConstraint
topConstraint = OWAConstraint {
  firstElementName = "button1",
  firstAttribute = Top,
  secondElementName = Just "Super",
  secondAttribute = Just Top,
  multiplier = 1.0,
  constant = 0
}

leftConstraint :: OWAConstraint
leftConstraint = OWAConstraint {
  firstElementName = "button1",
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
