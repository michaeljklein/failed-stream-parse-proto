# failed-stream-parse-proto

This library provides a simple model of parsing an AST through
successive transformations of an input stream.

It mostly provides types, e.g. implementations of code blocks or line comments,
with no implementations.

After writing a bit of the implementation and sketching how to build the idea into
a viable parsing library, I ended up dropping it in favor of a more flexible approach
involving a combination of `Cofree` and `Foldl`.

