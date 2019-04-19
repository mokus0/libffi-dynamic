libffi-dynamic
==============

A Haskell binding to the libffi library.

The novel thing about this implementation is that it defines a variadic function "dynamic" (and its inverse "wrap") that imports an arbitrarily-typed FunPtr as long as the types used as arguments and return are all instances of either the ArgType or RetType class, respectively.  Those classes can be implemented fairly easily, even for complex types such as structs.

Additionally, argument and return type mappings can be defined and manipulated explicitly to inductively build function type descriptions which can then be used to import or wrap functions.

Among other things, this makes it pretty straightforward to pass Haskell closures as callbacks to C APIs, or to import C functions with varargs or which might not have types that are known at the time the Haskell code is compiled (such as code that will by dynamically compiled and loaded later).

Documentation is mostly nonexistent at this point.  Sorry.  There is an example in the "examples" directory, at least.
