{-|
Module      : OWAView
Description : Module for view model and associated types, such as
              layout attributes and constraints.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAView where 

import OWAElements

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
  deriving (Show, Eq)

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

-- | OWAViewElement provides a wrapper around the different types of subviews
-- that a view can have. This will soon be expanded to include other OWAViews,
-- as well as simple container views.
data OWAViewElement = LabelElement OWALabel |
  TextFieldElement OWATextField |
  ButtonElement OWAButton |
  ImageElement OWAImageView
  deriving (Show, Eq)
