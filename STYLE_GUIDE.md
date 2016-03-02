# OWA Style Guide
For the sake of code consistency and ease of coding, follow these guidelines!

# General Code
1. One line break between any different parts of code. For instance, between methods, or after listing all imports, or after a section header comment. No lines between method comments and the methods themselves. 
2. Try to limit line length to 80 chars. 
3. Use tab width of 2, using spaces.
4. Start all lines all the way to the left except for indent spacing, except the next point:
5. When a list will cause a line to become too long, start a new line with the list at an incremented indentation level. Each element of the list should be on a new line, starting from the same indentation level. As an exception, the opening brace and first element should go on the preceding line if following '='.
6. When defining data types, if there is a single case, put it on one line along with any deriving statements. Otherwise start a new line after '=' and
put each case on its own line indented. Then put any deriving statements on their own line. 
7. Break source files up by section (see OWAFileSearch for an example). Public methods of each section should be at the top. 
8. Alphabetize imports.
9. Use Haddock comments for all public methods and modules in source code (not test code).
10. Use HLint, running `hlint .` from the root directory when you are ready to commit changes.

# Writing Test Specs
1. Only expose public methods of a test file (typically a single method like runXXXTests)
2. Put a comment at the top of the file listing the public methods being tested. 
3. These should be followed by methods defining specs. 
4. Next should come setup and teardown methods. 
5. Any helper methods can come next. 
6. Lastly include any constants.
7. If the test involves setting up a directory structure (e.g. AppDirectoryTests.hs), a comment showing the created structure can (and should) be included at the bottom of the file.
8. If a helper method can be useful elsewhere, it can go in TestUtil.hs.
