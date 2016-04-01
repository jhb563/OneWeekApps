module TestViews where

import OWAView

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
