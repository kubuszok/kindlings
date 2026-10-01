# Hearth gap: `DestructuredExpr` does not expose local `val` definitions

Status: **RESOLVED in Hearth 0.4.3** (kubuszok/hearth#385; found 2026-09-29 while implementing the `parser` module,
Hearth 0.4.2).

Hearth now has `ValDefinition`/`LocalReference` (sharing one `LocalBinding` instance), `Import` and `LocalDefinition`
nodes, `findReferences`/`Lambda.unusedParams` over the complete tree, and `position` on every node. The grammar block
is read by the shared `parser/src/main/scala/.../internal/compiletime/GrammarExtractor.scala`; the per-compiler
`GrammarMacros` keep only code generation.

## What is missing

The `parser` module's `Grammar.grammar[R, F] { g => import g._; val expr = nonTerminal[Int]; ...; expr ::= ...; expr }`
macro needs to read a block of statements: local `val` definitions, expression statements (method calls on those vals)
and references to the vals. `DestructuredExpr.parse` handles lambdas, blocks, method calls and literals, but:

- a `val x = rhs` statement inside a `Block` is not a `Term` on Scala 3 (`ValDef` is a `Statement`), and on both
  platforms it ends up as `DestructuredExpr.NonDestructurable` - the name, the symbol and the destructured `rhs` are not
  available;
- an identifier referring to such a local val (`Ident(x)`, not a lambda parameter and not a module) also ends up as
  `NonDestructurable`, so there is no way to link a use site to its definition;
- `import` statements inside the block are likewise opaque.

## Reproducer (sketch)

```scala
// in a macro, for `expr: Expr[Dsl[F] => NonTerminal[R]]` passed as `g => { import g._; val a = g.nonTerminal[Int]; a ::= ...; a }`
DestructuredExpr.parse(expr) match {
  case lam: DestructuredExpr.Lambda =>
    lam.body match {
      case b: DestructuredExpr.Block =>
        b.statements // List(NonDestructurable(import g._), NonDestructurable(val a = ...), MethodCall(::=, ...))
        b.result     // NonDestructurable(a)  <- cannot be linked to the `val a` statement
    }
}
```

## Former workaround in Kindlings

`parser/src/main/scala-{2,3}/.../internal/compiletime/GrammarMacros.scala` read the grammar block with the raw
compiler APIs (only the extraction into a platform-independent `GrammarIR`); analysis and table generation are shared.

## Suggested Hearth API

A `DestructuredExpr.ValDef(name, symbol/id, rhs: DestructuredExpr, declaredTpe)` statement node and a
`DestructuredExpr.LocalRef(valDef)` node for identifiers referring to vals defined earlier in the same destructured
tree (plus an opaque `Import` node), so that block-shaped DSLs can be read without platform-specific code.
