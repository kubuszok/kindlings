# Kindlings

[Hearth](https://github.com/MateuszKubuszok/hearth)-based type class derivation. To kindle more user-friendly libraries with metaprogramming under the hood.

## Rationale

 1. the dogfooding and the trial by fire of Hearth's utilities - if I cannot use Hearth to deliver something, noone else can.
    If there is an issue, I can learn about about it from a real world example and fix it before it affect anyone else.
 2. a proof by example that Hearth can have a practical applications - while not proof that is can always be used to replace a native macros
    and raw AST (which explicitly is not among Hearth's goals), it would show a variety of examples where macros can avoid raw ASTs,
    avoid manual handling of some difficult corner cases, cross-compile between Scala 2 and 3, and all of that can be accelerated
    by the usage of coding agents, which could code against high-level abstractions, that explicitly model intents, rather than low-level
    abstractions where we can only hope that the AST would generate the code that should work in all the cases
 3. a reference for learning how to use Hearth by studying complete, tested examples, rather than inferring use cases of various utilities
    from the documentation
