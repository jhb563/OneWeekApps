# OneWeekApps
OneWeekApps is being developed as a framework for rapidly developing mobile apps. Using Haskell, it uses parses user specifications into a an abstract syntax representing important pieces of a mobile application, and then prints the outputs to files which can be used within the mobile application. 

# Goals (V1)
1. Develop an abstract syntax for major pieces of a mobile application. Target pieces include views, controllers, coordinators, models, and certain repeatable elements such as colors, font, alerts, and notification systems.  
2. Develop an Domain Specific Language allowing developers to express the above pieces quickly and easily.
3. Use Haskell to parse the DSL into the abstract syntax.
4. Use Haskell to create the necessary files in Objective C with pretty printed code.
5. Create all necessary files to create the app project in XCode 

# Long Term Goals
1. Allow the user to modify their app structure and have changes reflected in XCode/AS project while respecting any additions made to the project.
2. Expand to android.

# Running the Program
1. To run the program, first make sure you have ghc and cabal installed.
2. Then in the project directory, creating the sandbox by running `cabal sandbox init`
3. Next install the project with `cabal install -j`
4. Finally run with .cabal-sandbox/bin/owa
5. It doesn't do very much yet!

# Generating Documentation
1. OWA uses Haddock for documentation.
2. To generate documentation, simply run `cabal haddock --executables`
