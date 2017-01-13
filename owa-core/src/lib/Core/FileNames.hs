module Core.FileNames where

-- | File where we store the last generation times for our different languages.
lastGenFileExtension :: FilePath
lastGenFileExtension = "/.owa_last_gen"

-- | The file where our app info is generated and loaded from
appInfoFileExtension :: FilePath
appInfoFileExtension = "/app.info"
