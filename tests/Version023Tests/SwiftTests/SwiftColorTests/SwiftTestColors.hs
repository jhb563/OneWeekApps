module SwiftTestColors where

import OWAColor

swiftTestColors :: [OWAColor]
swiftTestColors = map colorFromTuple [("color1", 178.0, 224.0, 67.0, 1.0),
                                    ("color2", 9.6, 255.0, 0.0, 1.0),
                                    ("purple", 255.0, 0.0, 255.0, 1.0),
                                    ("black", 0.0, 0.0, 0.0, 1.0),
                                    ("white", 255.0, 255.0, 255.0, 1.0),
                                    ("christmas", 127.758126, 164.6, 0.0, 1.0),
                                    ("christmas2", 127.758126, 164.6, 0.0, 1.0),
                                    ("aTest12", 245.0, 173.0, 122.0, 0.94),
                                    ("hex2", 222.0, 173.0, 192.0, 1.0),
                                    ("hexa2", 222.0, 173.0, 192.0, 1.0),
                                    ("hexAlpha2", 222.0, 173.0, 192.0, 1.0)]
