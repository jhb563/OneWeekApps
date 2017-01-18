module Parse.Tests.Fonts.Objects where

import Model.OWAFont

allTestFonts :: [OWAFont]
allTestFonts = [myFont,
  titleFont,
  labelFont,
  thinFont,
  placeholderFont,
  buttonFont,
  textfieldFont,
  randomFont,
  noStyleFont,
  styleInNameFont,
  timesFont]

sortedTestFonts :: [OWAFont]
sortedTestFonts = [buttonFont,
  labelFont,
  myFont,
  noStyleFont,
  placeholderFont,
  randomFont,
  styleInNameFont,
  textfieldFont,
  thinFont,
  timesFont,
  titleFont]

myFont :: OWAFont
myFont = OWAFont {
  fontName = "myFont",
  fontFamily = "HelveticaNeue",
  fontSize = 16.0,
  fontStyles = [Light]
}

titleFont :: OWAFont
titleFont = OWAFont {
  fontName = "titleFont",
  fontFamily = "Helvetica",
  fontSize = 32.0,
  fontStyles = [Bold]
}

labelFont :: OWAFont
labelFont = OWAFont {
  fontName = "labelFont",
  fontFamily = "SanFranciscoDisplay",
  fontSize = 14.5687,
  fontStyles = [Regular]
}

thinFont :: OWAFont
thinFont = OWAFont {
  fontName = "thinFont",
  fontFamily = "HelveticaNeue",
  fontSize = 14.02341,
  fontStyles = [Thin]
}

placeholderFont :: OWAFont
placeholderFont = OWAFont {
  fontName = "placeholderFont",
  fontFamily = "Verdana",
  fontSize = 0.5,
  fontStyles = [Italic]
}

buttonFont :: OWAFont
buttonFont = OWAFont {
  fontName = "buttonFont",
  fontFamily = "Avenir",
  fontSize = 12.0,
  fontStyles = [Medium]
}

textfieldFont :: OWAFont
textfieldFont = OWAFont {
  fontName = "textfieldFont",
  fontFamily = "Trebuchet",
  fontSize = 0.7,
  fontStyles = [Bold, Italic]
}

randomFont :: OWAFont
randomFont = OWAFont {
  fontName = "randomFont",
  fontFamily = "HelveticaNeue",
  fontSize = 13.6,
  fontStyles = [Medium, Italic]
}

noStyleFont :: OWAFont
noStyleFont = OWAFont {
  fontName = "noStyleFont",
  fontFamily = "GillSans",
  fontSize = 12.0,
  fontStyles = []
}

styleInNameFont :: OWAFont
styleInNameFont = OWAFont {
  fontName = "styleInNameFont",
  fontFamily = "Georgia-BoldItalic",
  fontSize = 13.2,
  fontStyles = []
}

timesFont :: OWAFont
timesFont = OWAFont {
  fontName = "timesFont",
  fontFamily = "TimesNewRomanPSMT",
  fontSize = 17.0,
  fontStyles = []
}
