# Find All References: Overrides — Design

Date: 2026-11-03 · Branch: `feature/find-all-references` (local) · Builds on the spike report `2026-11-02-xsharp-find-all-references-spike-report.md`

## Intent

FAR on a method or property also finds the calls that can reach it through inheritance, as in C#. Today
`oSub:Close()` resolves to `Sub.Close` when `Sub` overrides `Close`, so FAR on `Base.Close` misses it (and the other
way round).

## Goals (user: "wie in C#")

- FAR on a member of a class cascades to its **family**: the topmost member in the base-class chain with the same
  name and signature, and every member in a derived class that overrides it (siblings included).
- Works in both directions: FAR on `Sub.Close` also lists `Base.Close` and its references.
- One group per family member in the FAR window ("Method FarTest.Base.Close", "Method FarTest.Sub.Close"), each with
  its definition and its references.
- Methods and ACCESS/ASSIGN/PROPERTY.

## Non-goals

- Interface implementations, members of types from referenced assemblies, late-bound sends (set aside by the user).
- Constructors, functions, fields, locals (no overriding).

## Approaches

1. **Check the family per resolved candidate (chosen; the only sensible way here).** The name filter already brings
   every `Close` token; each resolved member is compared with the target by name, kind and parameter list, and its
   class is related to the target's class through the `BaseType` chain to the same topmost member. No index needed.
2. Precompute all derived types: needs a "derived types" query the code model does not have (no lookup by
   `BaseTypeName`); rejected.

## Decided (my calls, open to correction)

- `VIRTUAL`/`OVERRIDE` modifiers are ignored: same name + kind + parameter list counts (VO with `/vo3` makes every
  method virtual; a hiding `NEW` method is rare).
- Property-like members (ACCESS/ASSIGN/PROPERTY) match by name only, as `SymbolIdentity` already does.
- The search scope is computed from the class of the topmost family member (its project and every project that
  references it), so FAR on `Sub.Close` also searches the base class's project.

## Left to the builder

- How the window's buckets are keyed (one `DefinitionBucket` per family member, created on its first hit).
- Guarding the base chain against cycles and unresolved base types.
