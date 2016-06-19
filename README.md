# One Week Apps
One Week Apps is a template language for mobile development. The goal is to generate large amounts of tedious boilerplate code with clean and easy description of particular elements of a mobile application. 

# How does it work?
To generate code using OWA, you must first have a directory called “app”, and run the OWA executable from within or above this directory. OWA will then search the directory for files with specific extensions, such as .colors, .font, or .view. Depending on the extension, OWA will parse the file as a particular kind of element or collection of elements, and generate Objective C code in the app directory corresponding to those elements. You can then add the generated code into an XCode project to use it. 

# Running the Program
1. To run the program, first make sure you have the ghc platform and stack installed. 
2. In the project directory, you should be able to run `stack build`
3. The executable should be created in .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/OneWeekApps
4. It can be run from within the project directory by running `stack exec OneWeekApps`
5. Moving the executable onto your path will enable you to run `stack exec OneWeekApps` from anywhere. 
6. Run tests with `stack test`

# Features
You can generate code for categories for certain elements which are reusable throughout your app. These include colors, font, alerts, and errors. You can also create multiple files containing localized strings to be compiled into a single Localizable.strings file. 

The largest and most important feature is being able to generate code for your own UIView subclasses. You can specify elements within the view and use OWA’s easy layout syntax to generate Autolayout code for Objective C. Basic elements which are currently supported within views include Labels, Buttons, TextFields, ImageViews. You can also make your own custom views subviews of each other, place elements within vanilla UIViews, and embed elements within UIScrollViews. 

OWA also requires that you create a file called “app.info” containing some basic information about your app such as its name, your name as author, the app prefix, and so on. 

# Examples

To create a category for generic colors to use throughout your app, you can create a file called myapp.colors, containing the following text:

```
Color labelColor
	Red 10
	Green 10
	Blue 10

Color errorColor
	Red 123
	Green 15
	Blue 15
```

OWA will create files called UIColor+XXXColors.h, and UIColor+XXXColors.m, where XXX is the app prefix specified in app.info. These files will implement a category on UIColor exposing these generic colors:

```
@interface UIColor (XXXColors)

- (UIColor*)labelColor;
- (UIColor*)errorColor;

@end
```

And the .m file would handle implementing these using `[UIColor colorWithRed:green:blue:alpha]` 

See the tests folder for more examples. Tests for versions 0.2.0 and 0.2.1 focus on generation for view code. Earlier tests focus on generation for the generic app elements. 

# Future Goals
For the next release, the main goals are adding Swift code generation in addition to Objective C, and creating a better user experience with respect to adding generated code to XCode. See specs/spec-0.2.3.md for more details, or take a look at the Issues section on top! 

Longer term goals include generating code for more types of objects (such as View Controllers and Models) and beginning to expand to android code generation. 

# Feature Requests, Bug Fixes
If you spot a bug, create an issue for it, being sure to add the “Bug” label. If you have an idea for a feature, you can create an issue for that as well, just use the “Feature Request (Future)” label. Depending on the request it may get tracked into the current version. Of course if you implement the feature yourself and submit a Pull Request, this is much more likely! 

# Collaborating
If you are interested in helping out by fixing bugs or adding features, please do! You do not need to be a Haskell wizard to help out! See Collaborating.md for more details. If you fix a bug, be sure to add a log of it to  the bug_fixes file for the current version. 
