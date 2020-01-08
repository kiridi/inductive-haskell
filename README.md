# Inductive Haskell
This is the implementation that aims to support my 3rd year dissertation, _Machine learning functional prgrams_.
The implementation is still a prototype, so code is a bit messy and comments are lacking.

# Running the system
The only build dependency is __The Haskell Platform__, which will also install the _stack_ build system, used to run the program.

When in the project directory, running the command `stack run input` will synthesize a program based on the input file (relative path). For an example of how the input file should look like and what the syntax is, look at the file `test/droplasts.test`.
