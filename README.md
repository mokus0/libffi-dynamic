libffi-experiments
===================

As the name suggests, this is an experimental Haskell binding to the libffi library.

The novel thing about this implementation is that it defines a variadic function "dynamic" (and hopefully soon an inverse, "wrap") that imports an arbitrarily-typed FunPtr as long as the types used as arguments and return are all instances of either the ArgType or RetType class, respectively.  Those classes can be implemented fairly easily, even for complex types such as structs.

The code is fairly ugly at this point.  I threw this together pretty quickly and I'm not sure how much of the complexity is incidental.