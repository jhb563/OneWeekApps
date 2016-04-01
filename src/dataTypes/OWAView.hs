module OWAView where 

import OWAElements

data OWALayoutAttribute = Width |
  Height |
  LeftSide |
  RightSide |
  Top |
  Bottom |
  CenterX |
  CenterY
  deriving (Show, Eq)

data OWAConstraint = OWAConstraint {
  firstElement :: OWAViewElement,
  firstAttributeName :: String,
  secondElement :: Maybe OWAViewElement,
  secondAttributeName :: Maybe String,
  multiplier :: Float,
  constant :: Float
} deriving (Show, Eq)
  

data OWAView = OWAView {
  viewName :: String,
  viewType :: String,
  subviews :: [OWAViewElement],
  constraints :: [OWAConstraint]
} deriving (Show, Eq) 

data OWAViewElement = LabelElement OWALabel |
  TextFieldElement OWATextField |
  ButtonElement OWAButton |
  ImageElement OWAImageView
  deriving (Show, Eq)
