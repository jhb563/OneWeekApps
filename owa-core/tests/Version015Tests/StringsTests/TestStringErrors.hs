module TestStringErrors where

import Text.Parsec.Pos

keyFailError :: SourcePos
keyFailError = newPos "keyFailTest.strings" 1 1

wordFailError :: SourcePos
wordFailError = newPos "wordFailTest.strings" 2 14

quotesFailError :: SourcePos
quotesFailError = newPos "quotesFailTest.strings" 2 15

