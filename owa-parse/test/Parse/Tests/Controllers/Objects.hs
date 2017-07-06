module Parse.Tests.Controllers.Objects where

import Model.OWAController

basicController :: OWAController
basicController = OWAController
  { controllerTypeName = "OWARegistrationController"
  , controllerView = ViewType "OWARegistrationView"
  , controllerModelProperties = ("description", StringField) : producedProperties
  , controllerConnections =
    [ ButtonPressNavigation (ViewItemName "prevButton") (NavAction "popView" [])
    , ButtonPressNavigation
      (ViewItemName "nextButton")
      (NavAction "advanceView" producedProperties)
    ]
  , controllerNavProtocol = NavProtocol
      { navProtocolName = "RegistrationDelegate"
      , navProtocolMethods =
        [ NavAction (FunctionName "shouldPopController") []
        , NavAction (FunctionName "shouldAdvanceView") producedProperties
        ]
      }
  }
  where
    producedProperties = 
      [ ("email", StringField)
      , ("isMale", BoolField)
      , ("password", StringField)
      , ("username", StringField)
      ]

multiChoiceController :: OWAController
multiChoiceController = OWAController
  { controllerTypeName = "MultiChoiceController"
  , controllerView = ViewType "MultiChoiceView"
  , controllerModelProperties = props
  , controllerConnections =
    [ ButtonPressNavigation (ViewItemName "option1Button") (NavAction "chooseOption1" props)
    , ButtonPressNavigation (ViewItemName "option2Button") (NavAction "chooseOption2" props)
    , ButtonPressNavigation (ViewItemName "option3Button") (NavAction "chooseOption3" props)
    , ButtonPressNavigation (ViewItemName "prevButton") (NavAction "popView" [])
    ]
  , controllerNavProtocol = NavProtocol
      { navProtocolMethods = "MultiChoiceDelegate"
      , navProtocolMethods =
        [ NavAction (FunctionName "shouldPopController") []
        , NavAction (FunctionName "didChooseOption1") props
        , NavAction (FunctionName "didChooseOption2") props
        , NavAction (FunctionName "didChooseOption3") props
        ]
      }
  }
  where
    props = [("passThrough", MapType StringField)]

normalController :: OWAController
normalController = OWAController
  { controllerTypeName = "NormalController"
  , controllerView = "NormalView"
  , controllerModelProperties = props
  , controllerConnections =
    [ ButtonPressNavigation (ViewItemName "advancementButton") (NavAction "advanceView" props) ]
  , controllerNavProtocol = NavProtocol
    { navProtocolName = "NormalDelegate"
    , navProtocolMethods =
      [ NavAction (FunctionName "shouldPopController") []
      , NavAction (FunctionName "shouldAdvance") props
      ]
    }
  }
  where
    props = 
      [ ("myNumbers", ArrayType IntField)
      , ("myNumber", IntField)
      , ("myFlt", FloatField)
      , ("possibleName", OptionalType StringField)
      ]

textController :: OWAController
textController = OWAController
  { controllerTypeName = "TextController"
  , controllerView = "TextContainerView"
  , controllerProperties =
    [ ("firstEntry", StringField)
    , ("secondEntry", StringField)
    ]
  , controllerConnections =
    [ TextFieldReturnNextField (ViewItemName "firstField") (ViewItemName "secondField")
    , TextFieldFunction (ViewItemName "secondField") (FunctionName "evaluateFields")
    , ButtonPressNavigation (ViewItemName "doneButton") (NavAction "popView" [])
    ]
  , controllerNavProtocol = NavProtocol
    { navProtocolName = "TextControllerDelegate"
    , navProtocolMethods = [ NavAction (FunctionName "shouldPopController" []) ]
    }
  }
