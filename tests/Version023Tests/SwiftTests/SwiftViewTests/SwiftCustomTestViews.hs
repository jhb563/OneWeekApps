module SwiftCustomTestViews where

import OWAView
import OWAElements

basicCustomTest :: OWAView
basicCustomTest = OWAView {
  viewName = "myView",
  viewType = "IGACustomTest1",
  subviews = [LabelElement myLabel, 
              CustomViewElement basicFirstView, 
              ButtonElement myButton],
  constraints = [alignConstraintFromTuple ("myLabel", "Super", Top, 60),
                alignConstraintFromTuple ("myLabel", "Super", LeftSide, 30),
                heightWidthConstraintFromTuple ("myLabel", Nothing, 30, Height),
                heightWidthConstraintFromTuple ("myLabel", Nothing, 100, Width),
                belowConstraintFromTuple ("firstView", "myLabel", 0),
                alignConstraintFromTuple ("firstView", "myLabel", LeftSide, 0),
                heightWidthConstraintFromTuple ("firstView", Nothing, 100, Height),
                heightWidthConstraintFromTuple ("firstView", Nothing, 200, Width),
                belowConstraintFromTuple ("myButton", "firstView", 0),
                alignConstraintFromTuple ("myButton", "firstView", LeftSide, 10),
                heightWidthConstraintFromTuple ("myButton", Nothing, 50, Height),
                heightWidthConstraintFromTuple ("myButton", Nothing, 100, Width)]
}

myLabel :: OWALabel
myLabel = OWALabel {
  labelName = "myLabel",
  labelText = "Hi",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

basicFirstView :: OWAViewRecord
basicFirstView = OWAViewRecord {
  viewRecordName = "firstView",
  viewRecordType = "OWAFirstView"
}

myButton :: OWAButton
myButton = OWAButton {
  buttonName = "myButton",
  buttonText = Just "Hi",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

heightWidthConstraintFromTuple :: (String, Maybe String, Float, OWALayoutAttribute) -> OWAConstraint
heightWidthConstraintFromTuple (name, maybeSecondName, dimen, attr) = OWAConstraint {
  firstElementName = name,
  firstAttribute = attr,
  secondElementName = maybeSecondName,
  secondAttribute = case maybeSecondName of
                      Nothing -> Nothing
                      Just _ -> Just attr,
  multiplier = 1.0,
  constant = dimen
}

centerConstraintFromTuple :: (String, String, Bool) -> OWAConstraint
centerConstraintFromTuple (name, secondName, isX) = OWAConstraint {
  firstElementName = name,
  firstAttribute = attr,
  secondElementName = Just secondName,
  secondAttribute = Just attr,
  multiplier = 1.0,
  constant = 0
}
  where attr = if isX then CenterX else CenterY

alignConstraintFromTuple :: (String, String, OWALayoutAttribute, Float) -> OWAConstraint
alignConstraintFromTuple (name, secondName, attr, dimen) = OWAConstraint {
  firstElementName = name,
  firstAttribute = attr,
  secondElementName = Just secondName,
  secondAttribute = Just attr,
  multiplier = 1.0,
  constant = dimen
}

belowConstraintFromTuple :: (String, String, Float) -> OWAConstraint
belowConstraintFromTuple (name, secondName, dimen) = OWAConstraint {
  firstElementName = name,
  firstAttribute = Top,
  secondElementName = Just secondName,
  secondAttribute = Just Bottom,
  multiplier = 1.0,
  constant = dimen
} 
