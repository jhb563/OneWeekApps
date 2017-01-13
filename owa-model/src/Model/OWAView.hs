{-|
Module      : OWAView
Description : Module for view model and associated types, such as
              layout attributes and constraints.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Model.OWAView where 

import Model.OWAElements

-- | OWALayoutAttribute models the different aspects of a view or elements
-- that we can fix with constraints.
data OWALayoutAttribute = Width |
  Height |
  LeftSide |
  RightSide |
  Top |
  Bottom |
  CenterX |
  CenterY
  deriving (Eq)

-- | Show instance for attributes which gives us the more natural "Left"
-- and "Right" as opposed to LeftSide/RightSide, which are used to
-- avoid naming conflicts with the Haskell built in functions.
instance Show OWALayoutAttribute where
  show Width = "Width"
  show Height = "Height"
  show LeftSide = "Left"
  show RightSide = "Right"
  show Top = "Top"
  show Bottom = "Bottom"
  show CenterX = "CenterX"
  show CenterY = "CenterY"  

-- | OWAConstraint models the different constraints we can put between
-- different views and elements. 
data OWAConstraint = OWAConstraint {
  firstElementName :: String,
  firstAttribute :: OWALayoutAttribute,
  secondElementName :: Maybe String,
  secondAttribute :: Maybe OWALayoutAttribute,
  multiplier :: Float,
  constant :: Float
} deriving (Show, Eq)
  
-- | OWAView models any view for which the user wants to create their
-- own class files. They contain elements and constraints relating these
-- elements, as well as their own name and type. 
data OWAView = OWAView {
  viewName :: String,
  viewType :: String,
  subviews :: [OWAViewElement],
  constraints :: [OWAConstraint]
} deriving (Show, Eq) 

-- | OWAViewRecord simplifies the View model for cases where we
-- are unconcerned about the subviews and constraints. This occurs
-- for embedding custom views within our view.
data OWAViewRecord = OWAViewRecord {
  viewRecordName :: String,
  viewRecordType :: String
} deriving (Show, Eq)

-- | OWAContainer is another simplified View which just holds a group of
-- elements. It has a generic type, and does not store its own constraints.
data OWAContainer = OWAContainer {
  containerName :: String,
  containerBackgroundColorName :: Maybe String,
  containerSubviews :: [OWAViewElement]
} deriving (Show, Eq)

-- | OWAScrollDirection is an Enum for the possible directions a scroll view can go
data OWAScrollDirection = Vertical | Horizontal | Both deriving (Show, Eq)

-- | OWAScrollView holds a single container (which in turn has subviews)
-- and is designed to scroll
data OWAScrollView = OWAScrollView {
  scrollViewName :: String,
  scrollViewBackgroundColorName :: Maybe String,
  scrollDirection :: OWAScrollDirection,
  scrollViewContainer :: OWAContainer
} deriving (Show, Eq)

-- | OWAViewElement provides a wrapper around the different types of subviews
-- that a view can have. 
data OWAViewElement = LabelElement OWALabel |
  TextFieldElement OWATextField |
  ButtonElement OWAButton |
  ImageElement OWAImageView |
  CustomViewElement OWAViewRecord |
  ContainerViewElement OWAContainer |
  ScrollViewElement OWAScrollView
  deriving (Show, Eq)

-- | NameForElement provides a common interface for accessing the name of a
-- view element. We should perhaps make a typeclass for this instead. 
nameForElement :: OWAViewElement -> String
nameForElement (LabelElement label) = labelName label
nameForElement (TextFieldElement textField) = textFieldName textField
nameForElement (ButtonElement button) = buttonName button
nameForElement (ImageElement image) = imageViewName image
nameForElement (CustomViewElement record) = viewRecordName record
nameForElement (ContainerViewElement container) = containerName container
nameForElement (ScrollViewElement scrollView) = scrollViewName scrollView

-- | typeNameForElement provides a common interface for accessing the type
-- of a view element. This should also be done as a typeclass.
typeNameForElement :: OWAViewElement -> String
typeNameForElement (LabelElement _) = "UILabel"
typeNameForElement (TextFieldElement _) = "UITextField"
typeNameForElement (ButtonElement _) = "UIButton"
typeNameForElement (ImageElement _) = "UIImageView"
typeNameForElement (CustomViewElement record) = viewRecordType record
typeNameForElement (ContainerViewElement _) = "UIView"
typeNameForElement (ScrollViewElement _) = "UIScrollView"
