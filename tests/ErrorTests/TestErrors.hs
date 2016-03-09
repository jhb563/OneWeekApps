module TestErrors where

import OWAError

allTestErrors :: [OWAError]
allTestErrors = [myError1,
  disconnectError,
  authError,
  sadError,
  userNotFound,
  simpleError,
  noPrefixError,
  redundantDomain,
  differentDomain,
  beforeDomainSpec,
  whatError,
  whichError,
  interloperDomain,
  finalError]

myError1 :: OWAError
myError1 = OWAError {
  errorName = "myError1",
  errorDomain = "OWAFirstErrors",
  errorCode = "NetworkingError",
  errorDescription = "DESCRIPTION_KEY"
}

disconnectError :: OWAError
disconnectError = OWAError {
  errorName = "disconnectError",
  errorDomain = "OWAFirstErrors",
  errorCode = "DisconnectError",
  errorDescription = "DISCONNECTED!.&*"
}

authError :: OWAError
authError = OWAError {
  errorName = "authError",
  errorDomain = "OWALogin",
  errorCode = "AUTHERROR",
  errorDescription = "You can't \\\"Login\\\""
}

sadError :: OWAError
sadError = OWAError {
  errorName = "sadError",
  errorDomain = "OWAFirstErrors",
  errorCode = "tears_of_failure",
  errorDescription = "Crying about stuff"
}

userNotFound :: OWAError
userNotFound = OWAError {
  errorName = "userNotFound",
  errorDomain = "OWALogin",
  errorCode = "UserNotFound",
  errorDescription = "You didn't \\n make an account"
}

simpleError :: OWAError
simpleError = OWAError {
  errorName = "simpleError",
  errorDomain = "MyAppErrors",
  errorCode = "MyAppErrorsSimpleError",
  errorDescription = "SIMPLE_ERROR"
}

noPrefixError :: OWAError
noPrefixError = OWAError {
  errorName = "noPrefixError",
  errorDomain = "MyAppErrors",
  errorCode = "noPrefix",
  errorDescription = "I don't have a prefix"
}

redundantDomain :: OWAError
redundantDomain = OWAError {
  errorName = "redundantDomain",
  errorDomain = "MyAppErrors",
  errorCode = "MyAppErrorsredundantCode",
  errorDescription = "Domain Respecified"
}

differentDomain :: OWAError
differentDomain = OWAError {
  errorName = "differentDomain",
  errorDomain = "RandomDomain",
  errorCode = "random",
  errorDescription = "This error doesn't belong"
}

firstPrefixedError :: OWAError
firstPrefixedError = OWAError {
  errorName = "firstPrefixedError",
  errorDomain = "MyAppErrors",
  errorCode = "MyAppFirstError",
  errorDescription = "Use the different prefix"
}

unprefixedError :: OWAError
unprefixedError = OWAError {
  errorName = "unprefixedError",
  errorDomain = "MyAppErrors",
  errorCode = "SecondError",
  errorDescription = "Use no prefix"
}

beforeDomainSpec :: OWAError
beforeDomainSpec= OWAError {
  errorName = "beforeDomainSpec",
  errorDomain = "MyOwnDomain",
  errorCode = "MyOwnDomainSoleDomainCode",
  errorDescription = "This appears before anything with a domain"
}

whatError :: OWAError
whatError = OWAError {
  errorName = "whatError",
  errorDomain = "FirstDomain",
  errorCode = "belongsToFirstDomain",
  errorDescription = "Wait an error happened?"
}

whichError :: OWAError
whichError = OWAError {
  errorName = "whichError",
  errorDomain = "SecondDomain",
  errorCode = "SecondDomainWhichError",
  errorDescription = "This should be in second domain"
}

interloperDomain :: OWAError
interloperDomain = OWAError {
  errorName = "interloperDomain",
  errorDomain = "RandomDomain",
  errorCode = "InterloperDomainError",
  errorDescription = "Why not?"
}

finalError :: OWAError
finalError = OWAError {
  errorName = "finalError",
  errorDomain = "FirstDomain",
  errorCode = "FirstFinalError",
  errorDescription = "This is the last"
}
