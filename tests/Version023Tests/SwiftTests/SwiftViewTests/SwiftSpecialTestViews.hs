module SwiftSpecialTestViews where

import OWAView
import OWAElements

-- Custom Views

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

-- Container Tests

basicContainerTest :: OWAView
basicContainerTest = OWAView {
  viewName = "myView",
  viewType = "IGAContainerView1",
  subviews = [ContainerViewElement myContainer,
              ImageElement myImage],
  constraints = [alignConstraintFromTuple ("myContainer", "Super", Top, 40),
                alignConstraintFromTuple ("myContainer", "Super", LeftSide, 20),
                alignConstraintFromTuple ("myContainer", "Super", RightSide, -20),
                heightWidthConstraintFromTuple ("myContainer", Nothing, 200, Height),
                centerConstraintFromTuple ("nextLabel", "myContainer", True),
                centerConstraintFromTuple ("nextLabel", "myContainer", False),
                centerConstraintFromTuple ("nextButton", "myContainer", True),
                belowConstraintFromTuple ("nextButton", "nextLabel", 30),
                heightWidthConstraintFromTuple ("nextButton", Nothing, 30, Height),
                centerConstraintFromTuple ("myImage", "Super", True),
                belowConstraintFromTuple ("myImage", "myContainer", 30),
                heightWidthConstraintFromTuple ("myImage", Nothing, 80, Width), 
                heightWidthConstraintFromTuple ("myImage", Nothing, 80, Height)]
}

nestedContainerTest :: OWAView
nestedContainerTest = OWAView {
  viewName = "myView",
  viewType = "IGAContainerView2",
  subviews = [ContainerViewElement topContainer],
  constraints = [alignConstraintFromTuple ("topContainer", "Super", Top, 0),
                alignConstraintFromTuple ("topContainer", "Super", Bottom, 0),
                alignConstraintFromTuple ("topContainer", "Super", LeftSide, 0),
                alignConstraintFromTuple ("topContainer", "Super", RightSide, 0),
                alignConstraintFromTuple ("insideContainer", "topContainer", LeftSide, 0),
                alignConstraintFromTuple ("insideContainer", "topContainer", Top, 20),
                heightWidthConstraintFromTuple ("insideContainer", Nothing, 200, Height),
                heightWidthConstraintFromTuple ("insideContainer", Nothing, 200, Width),
                centerConstraintFromTuple ("insideLabel", "insideContainer", True),
                centerConstraintFromTuple ("insideLabel", "insideContainer", False),
                centerConstraintFromTuple ("topButton", "topContainer", False),
                toRightConstraint,
                heightWidthConstraintFromTuple ("topButton", Nothing, 30, Height),
                heightWidthConstraintFromTuple ("topButton", Nothing, 100, Width)]
}
  where toRightConstraint = OWAConstraint {
                              firstElementName = "topButton",
                              firstAttribute = LeftSide,
                              secondElementName = Just "insideContainer",
                              secondAttribute = Just RightSide,
                              multiplier = 1.0,
                              constant = 20
                            }

twoContainersTest :: OWAView
twoContainersTest = OWAView {
  viewName = "myView",
  viewType = "IGAContainerView3",
  subviews = [LabelElement firstLabel,
              ContainerViewElement container1,
              LabelElement secondLabel,
              ContainerViewElement container2],
  constraints = [alignConstraintFromTuple ("firstLabel", "Super", Top, 20),
                alignConstraintFromTuple ("firstLabel", "Super", LeftSide, 20),
                heightWidthConstraintFromTuple ("firstLabel", Nothing, 30, Height),
                heightWidthConstraintFromTuple ("firstLabel", Nothing, 100, Width),
                belowConstraintFromTuple ("container1", "firstLabel", 10),
                alignConstraintFromTuple ("container1", "firstLabel", LeftSide,  0),
                heightWidthConstraintFromTuple ("container1", Nothing, 100, Height),
                heightWidthConstraintFromTuple ("container1", Nothing, 100, Width),
                heightWidthConstraintFromTuple ("myImage", Just "container1", 0, Height),
                heightWidthConstraintFromTuple ("myImage", Just "container1", 0, Width),
                centerConstraintFromTuple ("myImage", "container1", True),
                centerConstraintFromTuple ("myImage", "container1", False),
                belowConstraintFromTuple ("secondLabel", "container1", 20),
                alignConstraintFromTuple ("secondLabel", "firstLabel", LeftSide, 0),
                heightWidthConstraintFromTuple ("secondLabel", Just "firstLabel", 0, Height),
                heightWidthConstraintFromTuple ("secondLabel", Just "firstLabel", 0, Width),
                belowConstraintFromTuple ("container2", "secondLabel", 0),
                alignConstraintFromTuple ("container2", "container1", LeftSide, 0),
                heightWidthConstraintFromTuple ("container2", Just "container1", 0, Height),
                heightWidthConstraintFromTuple ("container2", Just "container1", 0, Width),
                centerConstraintFromTuple ("myField", "container2", False),
                alignConstraintFromTuple ("myField", "container2", LeftSide, 0),
                alignConstraintFromTuple ("myField", "container2", RightSide, 0),
                heightWidthConstraintFromTuple ("myField", Nothing, 30, Height)]
}

myContainer :: OWAContainer
myContainer = OWAContainer {
  containerName = "myContainer",
  containerBackgroundColorName = Nothing,
  containerSubviews = [LabelElement nextLabel,
                      ButtonElement nextButton]
}

nextLabel :: OWALabel
nextLabel = OWALabel {
  labelName = "nextLabel",
  labelText = "Next",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

nextButton :: OWAButton
nextButton = OWAButton {
  buttonName = "nextButton",
  buttonText = Just "Next",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

myImage :: OWAImageView
myImage = OWAImageView {
  imageViewName = "myImage",
  imageSourceName = "file1.png"
}

topContainer :: OWAContainer
topContainer = OWAContainer {
  containerName = "topContainer",
  containerBackgroundColorName = Just "redColor",
  containerSubviews = [ContainerViewElement insideContainer,
                      ButtonElement topButton]
}

insideContainer :: OWAContainer
insideContainer = OWAContainer {
  containerName = "insideContainer",
  containerBackgroundColorName = Just "blueColor",
  containerSubviews = [LabelElement insideLabel]
}

insideLabel :: OWALabel
insideLabel = OWALabel {
  labelName = "insideLabel",
  labelText = "Hello from the inside",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

topButton :: OWAButton
topButton = OWAButton {
  buttonName = "topButton",
  buttonText = Just "Hello from not so inside",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
} 

firstLabel :: OWALabel
firstLabel = OWALabel {
  labelName = "firstLabel",
  labelText = "Name",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

container1 :: OWAContainer
container1 = OWAContainer {
  containerName = "container1",
  containerBackgroundColorName = Nothing,
  containerSubviews = [ImageElement myImage]
}

secondLabel :: OWALabel
secondLabel = OWALabel {
  labelName = "secondLabel",
  labelText = "Location",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

container2 :: OWAContainer
container2 = OWAContainer {
  containerName = "container2",
  containerBackgroundColorName = Nothing,
  containerSubviews = [TextFieldElement myField]
}

myField :: OWATextField
myField = OWATextField {
  textFieldName = "myField",
  textFieldText = Nothing,
  textFieldColorName = Nothing,
  textFieldFontName = Nothing,
  textFieldPlaceholderText = Just "Enter Location",
  textFieldPlaceholderTextColorName = Nothing,
  textFieldPlaceholderFontName = Nothing,
  textFieldBackgroundColorName = Nothing
}

-- Constraint Helpers

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
