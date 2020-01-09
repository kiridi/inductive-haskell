# Inductive Haskell
This is the implementation that aims to support my 3rd year dissertation, _Machine learning functional prgrams_.
The implementation is still a prototype, so code is a bit messy and comments are lacking.

Rigth now, the system is capable of finding programs that are part of a restricted program space. There are 3 hard coded metarules (a future version will allow the user to supply them) that describe how the functions should look like: function composition, map and filter (more will be addded). The input for those functions are either user defined background functions or functions that have been created through the synthesis process (functions whose name starts with _gen_). The language that we induce programs in is a pure language that maps to a subset of Haskell (a description could be found in the file _src/Language/Syntax.hs_ and an usage example can be found in the file _test/droplasts.test_).

Once a program has been successfully found, it will be pretty-printed in the console.

# Running the system
The only build dependency is __The Haskell Platform__, which will also install the _stack_ build system, used to run the program.

When in the project directory, running the command `stack run input` will synthesize a program based on the input file (relative path). For an example of how the input file should look like, look at the file `test/droplasts.test`.
