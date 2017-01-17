module Parse.Tests.Colors.Objects where

import Model.OWAColor

testColorsToPrint :: [OWAColor]
testColorsToPrint = rgbColorsList ++ mixFormatColorsList

rgbColorsList :: [OWAColor]
rgbColorsList = map colorFromTuple [("color1", 178.0, 224.0, 67.0, 1.0),
                                    ("color2", 9.6, 255.0, 0.0, 1.0),
                                    ("purple", 255.0, 0.0, 255.0, 1.0),
                                    ("black", 0.0, 0.0, 0.0, 1.0),
                                    ("white", 255.0, 255.0, 255.0, 1.0),
                                    ("christmas", 127.758126, 164.6, 0.0, 1.0)]

rgbaColorsList :: [OWAColor]
rgbaColorsList = map colorFromTuple [("aTest1", 245.0, 173.0, 122.0, 0.94),
                                    ("aTest2", 252.0, 253.0, 108.0, 0.91),
                                    ("aTest3", 85.0, 47.0, 81.0, 0.88),
                                    ("aTest4", 6.0, 75.0, 18.0, 0.15),
                                    ("aTest5", 220.0, 120.0, 5.0, 0.30),
                                    ("aTest6", 191.0, 176.0, 226.0, 0.76),
                                    ("aTest7", 118.0, 210.0, 12.0, 0.77),
                                    ("aTest8", 69.0, 111.0, 44.0, 0.08),
                                    ("aTest9", 19.0, 141.0, 167.0, 0.96),
                                    ("aTest10", 14.0, 152.0, 116.0, 0.18)]

rgbHexColorsList :: [OWAColor]
rgbHexColorsList = map colorFromTuple [("bTest1", 222.0, 173.0, 192.0, 1.0),
                                       ("bTest2", 222.0, 203.0, 17.0, 1.0),
                                       ("bTest3", 29.0, 43.0, 195.0, 1.0),
                                       ("bTest4", 244.0, 244.0, 79.0, 1.0),
                                       ("bTest5", 238.0, 244.0, 79.0, 1.0),
                                       ("bTest6", 53.0, 6.0, 80.0, 1.0)]

rgbaHexColorsList :: [OWAColor]
rgbaHexColorsList = map colorFromTuple [("cTest1", 222.0, 173.0, 192.0, 1.0),
                                        ("cTest2", 222.0, 203.0, 17.0, 52.0 / 255.0),
                                        ("cTest3", 29.0, 43.0, 195.0, 165.0 / 255.0),
                                        ("cTest4", 244.0, 244.0, 79.0, 195.0 / 255.0),
                                        ("cTest5", 238.0, 244.0, 79.0, 45.0 / 255.0),
                                        ("cTest6", 53.0, 6.0, 80.0, 152.0 / 255.0)]

hexAlphaColorsList :: [OWAColor]
hexAlphaColorsList = map colorFromTuple [("dTest1", 222.0, 173.0, 192.0, 1.0),
                                       ("dTest2", 222.0, 203.0, 17.0, 0.76),
                                       ("dTest3", 29.0, 43.0, 195.0, 0.56),
                                       ("dTest4", 244.0, 244.0, 79.0, 0.10),
                                       ("dTest5", 238.0, 244.0, 79.0, 0.75),
                                       ("dTest6", 53.0, 6.0, 80.0, 0.98)]

mixFormatColorsList :: [OWAColor]
mixFormatColorsList = map colorFromTuple [("christmas2", 127.758126, 164.6, 0.0, 1.0),
                                       ("aTest12", 245.0, 173.0, 122.0, 0.94),
                                       ("hex2", 222.0, 173.0, 192.0, 1.0),
                                       ("hexa2", 222.0, 173.0, 192.0, 1.0),
                                       ("hexAlpha2", 222.0, 173.0, 192.0, 1.0)]

