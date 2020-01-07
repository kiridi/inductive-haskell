# Inductive Haskell
This is the implementation that aims to support my 3rd year dissertation, _Machine learning functional prgrams_.
The implementation is still a prototype, so code is a bit messy and comments are lacking.

# Running the system
The only build dependency is __The Haskell Platform__, which will also install the _stack_ build system, used to run the program.

When in the project directory, running the command `stack run input` will synthesize a program based on the input file (relative path). For an example of how the input file should look like, look at the file `test/droplasts.test`. Also, the input is hard coded in the file `Interpreter.hs` (see the definition of `obey (Calculate (Synth ...)))` and modify the initial program and the bk functions that should be used from the input file).

Currently, the output is not a "pretty printed" program, but rather a list of functions that use converntions found at the beginning of the file `Interpreter.hs` (see the data types `IFunction` and `IProgram`).
