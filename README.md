# One Week Apps

## About OWA

One Week Apps is a template language for mobile development. The goal is to generate large amounts of tedious boilerplate code with clean and easy description of particular elements of a mobile application. 

## Website

[Check it out!](https://www.oneweekapps.com)

## Documentation

More detailed information can be found [here](https://www.oneweekapps.com/documentation).

## How does it work?

* To generate code using OWA, you must first have a directory called “app”, and run the OWA executable from within or above this directory. 
* OWA will then search the directory for files with specific extensions, such as .colors, .font, or .view files.
* Depending on the extension, OWA will parse the file as a particular kind of element or collection of elements, and generate Objective C code in the app directory corresponding to those elements.
* You can then add the generated code into an XCode project to use it. 

### Running the Program
1. To run the program, first install [GHC Platform](https://www.haskell.org/platform/) and [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
2. In the project directory, you should be able to run `stack build`
3. The executable should be created in .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/OneWeekApps
4. It can be run from within the project directory by running `stack exec OneWeekApps`
5. Moving the executable onto your path will enable you to run `stack exec OneWeekApps` from anywhere. 
6. Run tests with `stack test`

### Features
You can generate code for categories for certain elements which are reusable throughout your app. These include colors, font, alerts, and errors. You can also create multiple files containing localized strings to be compiled into a single Localizable.strings file. 

The largest and most important feature is being able to generate code for your own UIView subclasses. You can specify elements within the view and use OWA’s easy layout syntax to generate Autolayout code for Objective C. Basic elements which are currently supported within views include Labels, Buttons, TextFields, ImageViews. You can also make your own custom views subviews of each other, place elements within vanilla UIViews, and embed elements within UIScrollViews. 

Version 0.3.0 also introduced code generation for models files. See `owa-parse/tests` for details on the input format.

OWA also requires that you create a file called “app.info” containing some basic information about your app such as its name, your name as author, the app prefix, and so on. Since version 0.2.3, this file (and a sample XCode project) is created when you run the `owa new` command.

### Examples

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

See the tests folder for more examples.

## Support for Swift

Since version 0.2.3, all features have been supported for Swift as well as Objective C. Note that running the `owa new` command will generate separate XCode projects, one for Objective C and one for Swift.

## Future Goals
For the next release (0.3.1), the main goals are adding code generation for View Controllers and Coordinators, which allow navigation. Once this is done, it should be possibly to create a fully fledged app.

Longer term goals include generating code for more service oriented tasks like network requests. At some point, we will hopefully look at expanding to android code generation. 

## Bug Fixes and Feature Requests
If you spot a bug, [create an issue](https://github.com/jhb563/OneWeekApps/issues/new) for it and **be sure to add the “Bug” label**. If you have an idea for a feature, you can [create an issue](https://github.com/jhb563/OneWeekApps/issues/new) for that as well, just **use the “Feature Request” label**. Depending on the request it may get tracked into the current version. Of course if you implement the feature yourself and submit a Pull Request, this is much more likely! 

## Collaborating
If you are interested in helping out by fixing bugs or adding features, please do! You do not need to be a Haskell wizard to help out! See the [collaborating guide](COLLABORATING.md) for more details. If you fix a bug, be sure to add a log of it to  the bug_fixes file for the current version. 
