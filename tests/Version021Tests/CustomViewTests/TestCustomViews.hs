module TestCustomViews where

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

twoSameCustomTest :: OWAView
twoSameCustomTest = OWAView {
  viewName = "myView",
  viewType = "IGACustomTest2",
  subviews = [CustomViewElement custom1,
              CustomViewElement custom2],
  constraints = [alignConstraintFromTuple ("custom1", "Super", Top, 40),
                centerConstraintFromTuple ("custom1", "Super", True),
                heightWidthConstraintFromTuple ("custom1", Nothing, 200, Width),
                heightWidthConstraintFromTuple ("custom1", Nothing, 200, Height),
                belowConstraintFromTuple ("custom2", "custom1", 0),
                centerConstraintFromTuple ("custom2", "custom1", True),
                heightWidthConstraintFromTuple ("custom2", Just "custom1", 0, Width),
                heightWidthConstraintFromTuple ("custom2", Just "custom1", 0, Height)]
}

custom1 :: OWAViewRecord
custom1 = OWAViewRecord {
  viewRecordName = "custom1",
  viewRecordType = "OWACustomView"
}

custom2 :: OWAViewRecord
custom2 = OWAViewRecord {
  viewRecordName = "custom2",
  viewRecordType = "OWACustomView"
}

twoDifferentCustomTest :: OWAView
twoDifferentCustomTest = OWAView {
  viewName = "myView",
  viewType = "IGACustomTest3",
  subviews = [CustomViewElement firstView, 
              ButtonElement aButton, 
              CustomViewElement secondView],
  constraints = [centerConstraintFromTuple ("firstView", "Super", True),
                centerConstraintFromTuple ("firstView", "Super", False),
                heightWidthConstraintFromTuple ("firstView", Nothing, 100, Height),
                heightWidthConstraintFromTuple ("firstView", Nothing, 100, Width),
                leftConstraint,
                centerConstraintFromTuple ("aButton", "firstView", False),
                heightWidthConstraintFromTuple ("aButton", Nothing, 30, Height),
                heightWidthConstraintFromTuple ("aButton", Nothing, 50, Width),
                belowConstraintFromTuple ("secondView", "firstView", 0),
                centerConstraintFromTuple ("secondView", "firstView", True),
                heightWidthConstraintFromTuple ("secondView", Nothing, 50, Height),
                heightWidthConstraintFromTuple ("secondView", Nothing, 100, Width)]
}
  where leftConstraint = OWAConstraint {
                            firstElementName = "aButton",
                            firstAttribute = RightSide,
                            secondElementName = Just "firstView",
                            secondAttribute = Just LeftSide,
                            multiplier = 1.0,
                            constant = 0
                          }

firstView :: OWAViewRecord
firstView = OWAViewRecord {
  viewRecordName = "firstView",
  viewRecordType = "OWAFirstView"
}
aButton :: OWAButton
aButton = OWAButton {
  buttonName = "aButton",
  buttonText = Just "Hi",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
} 

secondView :: OWAViewRecord
secondView = OWAViewRecord {
  viewRecordName = "secondView",
  viewRecordType = "OWASecondView"
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
