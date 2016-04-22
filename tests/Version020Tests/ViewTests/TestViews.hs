module TestViews where

import OWAView
import OWAElements

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
  buttonText = "CLICK_ME",
  buttonTextColorName = Nothing,
  buttonFontName = Nothing,
  buttonBackgroundColorName = Nothing
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
  buttonText = "NEXT",
  buttonTextColorName = Just "view2Color",
  buttonFontName = Just "standardFont",
  buttonBackgroundColorName = Nothing
}

prevButton :: OWAButton
prevButton = OWAButton {
  buttonName = "prevButton",
  buttonText = "PREV",
  buttonTextColorName = Just "view2Color",
  buttonFontName = Just "placeholderFont",
  buttonBackgroundColorName = Just "elementBackgroundColor"
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

emptyLayoutTestView :: OWAView
emptyLayoutTestView = elementTest1
