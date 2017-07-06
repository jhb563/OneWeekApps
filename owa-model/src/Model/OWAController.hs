{-|
Module      : OWAModel
Description : Module for model model. We will use these to create class objects.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Model.OWAController where

import Model.OWAModel (OWAModelFieldType)

-- | Newtype wrapper for names that refer to elements in the view, like buttons and text fields.
newtype ViewItemName = ViewItemName String

-- | Newtype wrapper for names of functions that connections might call.
newtype FunctionName = FunctionName String

-- | Newtype wrapper for the type of the controller's view.
newtype ViewType = ViewType String

-- | Represents a "Controller", corresponding to UIViewController in iOS. These objects
-- handle tasks like presenting the view and managing the connections between the view and
-- the various model objects. They also store a list of model properties that may be passed.
-- They have knowledge of their own protocol for navigating away from themselves.
data OWAController = OWAController
  { controllerTypeName :: String
  , controllerView :: ViewType
  , controllerConnection :: [ViewConnection]
  , controllerModelProperties :: [(String, OWAModelFieldType)]
  , controllerNavProtocol :: NavProtocol
  }

-- | A connection between the view and the controller. There are specific varieties here
-- dealing with navigation actions. Others will simply call normal functions.
data ViewConnection =
  ButtonPressNavigation ViewItemName NavAction |
  ButtonPressFunction ViewItemName FunctionName |
  TextFieldReturnNextField ViewItemName ViewItemName |
  TextFieldReturnNavigation ViewItemName NavAction |
  TextFieldFunction ViewItemName FunctionName
    
-- | A protocol for communicating with a coordinator about the different ways we can move away
-- from this view.
data NavProtocol = NavProtocol
  { navProtocolName :: String
  , navProtocolMethods :: [NavAction]
  }

-- | A specific action, which knows its function name the properties it produces. The
-- coordinator will determine exactly what happens when this action runs. 
data NavAction = NavAction
  { navActionFunctionName :: FunctionName
  , navActionProducedProperties :: [(String, OWAModelFieldType)]
  }
