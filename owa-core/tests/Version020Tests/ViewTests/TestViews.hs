module TestViews where

import Model.OWAElements
import Model.OWAView

nameTest1 :: OWAView
nameTest1 = OWAView {
  viewName = "myView",
  viewType = "VIANameTestView",
  subviews = [],
  constraints = []
}

nameTest2 :: OWAView
nameTest2 = OWAView {
  viewName = "container2",
  viewType = "VIANameTestView",
  subviews = [],
  constraints = []
}

nameTest3 :: OWAView
nameTest3 = OWAView {
  viewName = "containerView2",
  viewType = "VIAPrimaryView",
  subviews = [],
  constraints = []
}

nameTest4 :: OWAView
nameTest4 = OWAView {
  viewName = "secondaryView",
  viewType = "VIASecondaryView",
  subviews = [],
  constraints = []
}

nameTest5 :: OWAView
nameTest5 = OWAView {
  viewName = "commentContainer",
  viewType = "myComment",
  subviews = [],
  constraints = []
}

viewLabel :: OWALabel
viewLabel = OWALabel {
  labelName = "viewLabel",
  labelText = "TITLE",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

viewTextField :: OWATextField
viewTextField = OWATextField {
  textFieldName = "viewField",
  textFieldText = Just "HELLO_WORLD",
  textFieldColorName = Just "view1TextFieldColor",
  textFieldFontName = Just "standardFont",
  textFieldPlaceholderText = Nothing,
  textFieldPlaceholderTextColorName = Nothing,
  textFieldPlaceholderFontName = Nothing,
  textFieldBackgroundColorName = Nothing
}

viewButton :: OWAButton
viewButton = OWAButton {
  buttonName = "viewButton",
  buttonText = Just "CLICK_ME",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

viewImage :: OWAImageView
viewImage = OWAImageView {
  imageViewName = "viewImage",
  imageSourceName = "profileImage.png"
}

elementTest1 :: OWAView
elementTest1 = OWAView {
  viewName = "myView",
  viewType = "VIAElementTest1",
  subviews = [TextFieldElement viewTextField,
    ButtonElement viewButton,
    ImageElement viewImage,
    LabelElement viewLabel],
  constraints = []
}

firstLabel :: OWALabel
firstLabel = OWALabel {
  labelName = "firstLabel",
  labelText = "FIRST",
  labelTextColorName = Just "view1LabelColor",
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

secondLabel :: OWALabel
secondLabel = OWALabel {
  labelName = "secondLabel",
  labelText = "SECOND",
  labelTextColorName = Nothing,
  labelFontName = Just "placeholderFont",
  labelBackgroundColorName = Just "elementBackgroundColor"
}

firstTextfield :: OWATextField
firstTextfield = OWATextField {
  textFieldName = "firstTextfield",
  textFieldText = Nothing,
  textFieldColorName = Nothing,
  textFieldFontName = Nothing,
  textFieldPlaceholderText = Just "ENTER_NAME",
  textFieldPlaceholderTextColorName = Just "view1TextFieldColor",
  textFieldPlaceholderFontName = Just "placeholderFont",
  textFieldBackgroundColorName = Nothing
}

secondTextfield :: OWATextField
secondTextfield = OWATextField {
  textFieldName = "secondTextfield",
  textFieldText = Nothing,
  textFieldColorName = Just "view2Color",
  textFieldFontName = Just "standardFont",
  textFieldPlaceholderText = Just "ENTER_LOCATION",
  textFieldPlaceholderTextColorName = Just "placeholderColor",
  textFieldPlaceholderFontName = Just "placeholderFont",
  textFieldBackgroundColorName = Nothing
}

nextButton :: OWAButton
nextButton = OWAButton {
  buttonName = "nextButton",
  buttonText = Just "NEXT",
  buttonTextColorName = Just "view2Color",
  buttonFontName = Just "standardFont",
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

prevButton :: OWAButton
prevButton = OWAButton {
  buttonName = "prevButton",
  buttonText = Just "PREV",
  buttonTextColorName = Just "view2Color",
  buttonFontName = Just "placeholderFont",
  buttonBackgroundColorName = Just "elementBackgroundColor",
  buttonBackgroundImageSourceName = Nothing
}

firstImage :: OWAImageView
firstImage = OWAImageView {
  imageViewName = "firstImage",
  imageSourceName = "file1.png"
}

secondImage :: OWAImageView
secondImage = OWAImageView {
  imageViewName = "secondImage",
  imageSourceName = "file2.png"
}

elementTest2 :: OWAView
elementTest2 = OWAView {
  viewName = "containerView",
  viewType = "VIAElementTest2",
  subviews = [LabelElement firstLabel,
    TextFieldElement firstTextfield,
    LabelElement secondLabel,
    ButtonElement nextButton,
    ButtonElement prevButton,
    ImageElement firstImage,
    TextFieldElement secondTextfield,
    ImageElement secondImage],
  constraints = []
}

lab :: OWALabel
lab = OWALabel {
  labelName = "lab",
  labelText = "HERE_IS_YOUR_NAME",
  labelTextColorName = Just "labelTextColor",
  labelFontName = Just "standardFont",
  labelBackgroundColorName = Just "elementBackgroundColor"
}

txt :: OWATextField
txt = OWATextField {
  textFieldName = "txt",
  textFieldText = Nothing,
  textFieldColorName = Just "blueColor",
  textFieldFontName = Just "standardFont",
  textFieldPlaceholderText = Nothing,
  textFieldPlaceholderTextColorName = Nothing,
  textFieldPlaceholderFontName = Nothing,
  textFieldBackgroundColorName = Just "redColor"
}

elementTest3 :: OWAView
elementTest3 = OWAView {
  viewName = "finalView",
  viewType = "VIAElementTest3",
  subviews = [LabelElement lab,
    TextFieldElement txt],
  constraints = []
}

noLayoutTestView :: OWAView
noLayoutTestView = elementTest1

heightWidthTestView :: OWAView
heightWidthTestView = OWAView {
  viewName = "myView",
  viewType = "VIAConstraintTest2",
  subviews = [TextFieldElement viewTextField,
    ButtonElement viewButton,
    ImageElement viewImage,
    LabelElement viewLabel],
  constraints = [textFieldHeight,
    textFieldWidth,
    buttonHeight,
    buttonWidth,
    imageHeight,
    imageWidth,
    labelHeight,
    labelWidth]
}

textFieldHeight :: OWAConstraint
textFieldHeight = OWAConstraint {
  firstElementName = "viewField",
  firstAttribute = Height,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 30.0
}

textFieldWidth :: OWAConstraint
textFieldWidth = OWAConstraint {
  firstElementName = "viewField",
  firstAttribute = Width,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 100.0
}

buttonHeight :: OWAConstraint
buttonHeight = OWAConstraint {
  firstElementName = "viewButton",
  firstAttribute = Height,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 50.0
}

buttonWidth :: OWAConstraint
buttonWidth = OWAConstraint {
  firstElementName = "viewButton",
  firstAttribute = Width,
  secondElementName = Nothing,
  secondAttribute = Nothing,
  multiplier = 1.0,
  constant = 200
}

imageHeight :: OWAConstraint
imageHeight = OWAConstraint {
  firstElementName = "viewImage",
  firstAttribute = Height,
  secondElementName = Just "viewButton",
  secondAttribute = Just Height,
  multiplier = 1.0,
  constant = 0.0
}

imageWidth :: OWAConstraint
imageWidth = OWAConstraint {
  firstElementName = "viewImage",
  firstAttribute = Width,
  secondElementName = Just "viewButton",
  secondAttribute = Just Width,
  multiplier = 1.0,
  constant = 0.0
}

labelHeight :: OWAConstraint
labelHeight = OWAConstraint {
  firstElementName = "viewLabel",
  firstAttribute = Height,
  secondElementName = Just "viewField",
  secondAttribute = Just Height,
  multiplier = 1.0,
  constant = 5.0
}

labelWidth :: OWAConstraint
labelWidth = OWAConstraint {
  firstElementName = "viewLabel",
  firstAttribute = Width,
  secondElementName = Just "viewField",
  secondAttribute = Just Width,
  multiplier = 1.0,
  constant = -5.0
}

alignTestView1 :: OWAView
alignTestView1 = OWAView {
  viewName = "constraintTest3",
  viewType = "VIAConstraintTest3",
  subviews = [l1, b1, t1, i1, l2, b2, t2, i2],
  constraints = map alignConstraintFromTuple
    [("l1", "Super", Top, 0),
    ("l1", "Super", RightSide, 0),
    ("b1", "Super", Top, 10.0),
    ("b1", "Super", RightSide, 10.0),
    ("t1", "Super", Top, 5.6),
    ("t1", "Super", LeftSide, 5.6),
    ("i1", "Super", LeftSide, 0),
    ("i1", "Super", Top, 0),
    ("l2", "Super", LeftSide, -5.0),
    ("l2", "Super", Bottom, -5.0),
    ("b2", "Super", Bottom, 0.0),
    ("b2", "Super", LeftSide, 0.0),
    ("t2", "Super", RightSide, 0.0),
    ("t2", "Super", Bottom, 0.0),
    ("i2", "Super", Bottom, 14.43112),
    ("i2", "Super", RightSide, 3.743)] 
}

alignTestView2 :: OWAView
alignTestView2 = OWAView {
  viewName = "constraintTest4",
  viewType = "VIAConstraintTest4",
  subviews = [iBase1, iBase2, l1, b1, t1, i1, l2, b2, t2, i2],
  constraints = map alignConstraintFromTuple
    [("l1", "iBase1", Top, 0),
    ("l1", "iBase1", RightSide, 0),
    ("b1", "iBase1", Top, 14.3),
    ("b1", "iBase1", RightSide, 0.0),
    ("t1", "iBase1", Top, 12.5),
    ("t1", "iBase1", LeftSide, -3.4),
    ("i1", "iBase1", LeftSide, 0),
    ("i1", "iBase1", Top, 0.0),
    ("l2", "iBase2", Bottom, -1.0),
    ("l2", "iBase2", LeftSide, -1.0),
    ("b2", "iBase2", Bottom, 0.0),
    ("b2", "iBase2", LeftSide, 0.0),
    ("t2", "iBase2", Bottom, 0.0),
    ("t2", "iBase2", RightSide, 0.0),
    ("i2", "iBase2", Bottom, 1.0),
    ("i2", "iBase2", RightSide, 1.0)] 
}

l1 :: OWAViewElement
l1 = LabelElement OWALabel {
  labelName = "l1",
  labelText = "Hi",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

l2 :: OWAViewElement
l2 = LabelElement OWALabel {
  labelName = "l2",
  labelText = "Bye",
  labelTextColorName = Nothing,
  labelFontName = Nothing,
  labelBackgroundColorName = Nothing
}

b1 :: OWAViewElement
b1 = ButtonElement OWAButton {
  buttonName = "b1",
  buttonText = Just "Hi",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

b2 :: OWAViewElement
b2 = ButtonElement OWAButton {
  buttonName = "b2",
  buttonText = Just "Bye",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing,
  buttonBackgroundImageSourceName = Nothing
}

t1 :: OWAViewElement
t1 = TextFieldElement OWATextField {
  textFieldName = "t1",
  textFieldText = Just "Hi",
  textFieldColorName = Nothing,
  textFieldFontName = Nothing,
  textFieldPlaceholderText = Nothing,
  textFieldPlaceholderTextColorName = Nothing,
  textFieldPlaceholderFontName = Nothing,
  textFieldBackgroundColorName = Nothing
}

t2 :: OWAViewElement
t2 = TextFieldElement OWATextField {
  textFieldName = "t2",
  textFieldText = Just "Bye",
  textFieldColorName = Nothing,
  textFieldFontName = Nothing,
  textFieldPlaceholderText = Nothing,
  textFieldPlaceholderTextColorName = Nothing,
  textFieldPlaceholderFontName = Nothing,
  textFieldBackgroundColorName = Nothing
}

i1 :: OWAViewElement
i1 = ImageElement OWAImageView {
  imageViewName = "i1",
  imageSourceName = "file1.png"
}

i2 :: OWAViewElement
i2 = ImageElement OWAImageView {
  imageViewName = "i2",
  imageSourceName = "file2.png"
}

iBase1 :: OWAViewElement
iBase1 = ImageElement OWAImageView {
  imageViewName = "iBase1",
  imageSourceName = "file1.png"
}

iBase2 :: OWAViewElement
iBase2 = ImageElement OWAImageView {
  imageViewName = "iBase2",
  imageSourceName = "file2.png"
}

alignConstraintFromTuple :: (String, String, OWALayoutAttribute, Float) -> OWAConstraint
alignConstraintFromTuple (v1, v2, attr, val) = OWAConstraint {
  firstElementName = v1,
  firstAttribute = attr,
  secondElementName = Just v2,
  secondAttribute = Just attr,
  multiplier = 1.0,
  constant = val
}

placementTestView :: OWAView
placementTestView = OWAView {
  viewName = "constraintTest5",
  viewType = "VIAConstraintTest5",
  subviews = [l1, b1, t1, i1, l2, b2, t2, i2],
  constraints = map placementConstraintFromTuple
    [("b1", "l1", Top, 0.0),
    ("t1", "l1", LeftSide, 0.0),
    ("i1", "b1", LeftSide, 10.0),
    ("i1", "t1", Top, 10.0),
    ("l2", "b2", Bottom, 0.0),
    ("l2", "t2", RightSide, 0.0),
    ("b2", "i2", RightSide, 5.0),
    ("t2", "i2", Bottom, 6.6)]
}

placementConstraintFromTuple :: (String, String, OWALayoutAttribute, Float) -> OWAConstraint
placementConstraintFromTuple (v1, v2, attr, val) = OWAConstraint {
  firstElementName = v1,
  firstAttribute = attr,
  secondElementName = Just v2,
  secondAttribute = reverseAttr,
  multiplier = 1.0,
  constant = val
}
  where reverseAttr = case attr of
                        Top -> Just Bottom
                        Bottom -> Just Top
                        RightSide -> Just LeftSide
                        LeftSide -> Just RightSide
                        _ -> Nothing

centerTestView :: OWAView
centerTestView = OWAView {
  viewName = "constraintTest6",
  viewType = "VIAConstraintTest6",
  subviews = [l1, b1, t1, i1],
  constraints = map centerConstraintFromTuple
    [("l1", "Super", CenterX, 0.0),
    ("l1", "Super", CenterY, 0.0),
    ("b1", "Super", CenterX, 10.0),
    ("b1", "Super", CenterY, -10.0),
    ("t1", "l1", CenterX, 0.0),
    ("t1", "b1", CenterY, 0.0),
    ("i1", "b1", CenterX, 5.5),
    ("i1", "l1", CenterY, 3.2)]
}

centerConstraintFromTuple :: (String, String, OWALayoutAttribute, Float) -> OWAConstraint
centerConstraintFromTuple (v1, v2, attr, val) = OWAConstraint {
  firstElementName = v1,
  firstAttribute = attr,
  secondElementName = Just v2,
  secondAttribute = Just attr,
  multiplier = 1.0,
  constant = val
}
