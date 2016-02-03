# OWA Style Guide
For the sake of code consistency and ease of coding, follow these guidelines!

# General Code
1. One line break between any different parts of code. For instance, between methods, or after listing all imports, or after a section header comment. No lines between method comments and the methods themselves. 
2. Try to limit line length to 80 chars. 
3. Use tab width of 2, using spaces.
4. Start all lines all the way to the left except for indent spacing, except the next point:
5. If it is feasible and doesn't make a line too long, lists can have their different elements at an equal indentation level. 
6. Break source files up by section (see OWAFileSearch for an example). Public methods of each section should be at the top. 
7. Use Haddock comments for all public methods and modules in source code (not test code).
8. Use HLint. 

# Writing Test Specs
1. Only expose public methods of a test file (typically a single method like runXXXTests)
2. Put a comment at the top of the file listing the public methods being tested. 
3. These should be followed by methods defining specs. 
4. Next should come setup and teardown methods. 
5. Any helper methods can come next. 
6. Lastly include any constants.
7. If the test involves setting up a directory structure (e.g. AppDirectoryTests.hs), a comment showing the created structure can (and should) be included at the bottom of the file.
8. If a helper method can be useful elsewhere, it can go in TestUtil.hs.
