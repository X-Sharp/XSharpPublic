# Find All References: Constructors — Design

Date: 2026-11-03 · Branch: `feature/find-all-references` (local) · Builds on the spike report `2026-11-02-xsharp-find-all-references-spike-report.md`

## Intent

Find All References (FAR) on a constructor lists where objects are created and where the constructor is chained, as in
C#. FAR on a class also lists the `Foo{...}` creations. Today a constructor's name in the code model is `.ctor`, so the
name filter finds nothing, and `Foo{...}` resolves to the constructor, so FAR on the class misses it.

## Goals

- FAR on `CONSTRUCTOR(...)` (declaration) or on a call site lists:
  - `Foo{...}` and `Ns.Foo{...}` creations,
  - `SUPER(...)` in constructors of derived classes,
  - `SELF(...)` in constructors of the same class,
  - the declaration itself (the `CONSTRUCTOR` keyword) as definition.
- Overloads are told apart with the existing `OverloadFilter` (count, then type).
- FAR on a class name also lists `Foo{...}` (a resolved constructor of that class counts as a reference to the class).
- FAR on `Foo{...}` of a class without a declared constructor searches the class.

## Non-goals

- `METHOD Init` / `SUPER:Init(...)` with `/vo1` (user: not needed).
- Constructors from referenced assemblies, static constructors, primary-constructor parameter lists.

## Approaches

1. **Extend the existing search (chosen by the user).** For a constructor target the candidate scanner also yields
   type-name tokens followed by `{`, `SUPER`/`SELF` followed by `(`, and the `CONSTRUCTOR` keyword; each goes through
   the lookup, the overload filter and `SymbolIdentity`. The text pre-filter is the class name (a derived class names
   it in `INHERIT`). Cost: one resolution per `SUPER(`/`SELF(` in files that mention the class.
2. Syntactic only, no resolution: faster, but mixes same-named classes and overloads. Rejected.

## Decided

- Text filter for a constructor target: the declaring type's short name.
- Declaration detection: the `CONSTRUCTOR` keyword token of a constructor entity is that entity's declared name.
- Identity of constructors uses the existing rules (kind + declaring type + parameter list for database members).

## Left to the builder

- Where the target's short type name comes from (`ParentType`, `DeclaringType`).
- How the synthetic constructor of the lookup (class without constructors) is turned into the class target.
- Accepted gap: a file that calls a constructor without containing the class name anywhere (only conceivable via a
  `USING` alias) is not searched.
