# Find All References: Constructors — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** FAR on a constructor lists `Foo{...}`, `SUPER(...)`, `SELF(...)` and its declaration; FAR on a class also lists `Foo{...}`.

**Architecture:** Extend the existing pipeline in `src/VisualStudio/LanguageService/FindReferences`: the candidate scanner
yields constructor call sites, `OverloadFilter` reads `{...}` argument lists and keyword names, `SymbolIdentity` decides
whether a resolved symbol refers to the target, the command maps a synthetic constructor to its class.

**Tech Stack:** C# (net472 / VS 2022 language service), xUnit (`LanguageService.Tests`, references the built DLL).

**Spec:** `docs/superpowers/specs/2026-11-03-far-constructors-design.md`

## Global Constraints

- No `METHOD Init` / `/vo1` handling.
- CRLF, UTF-8 BOM, Edit tool; tests need `LanguageService2022.csproj` built first (Debug, `-p:Platform=AnyCPU`, `-p:SolutionDir=…/src/`).

## Review Focus

- `SUPER:Method()` / `SELF:Method()` (colon, not paren) must not become constructor candidates — pinned in Task 1.
- `Foo{}` where `Foo` is the target class but resolves to a constructor of another `Foo` (other namespace) — identity via
  declaring type full name, pinned in Task 2.
- A `CONSTRUCTOR` keyword of another class in the same file is a declaration of something else, not a hit — Task 3.

---

### Task 1: Candidates and arguments for constructor call sites

**Files:** Modify `FindReferences/ReferenceCandidateScanner.cs`, `FindReferences/OverloadFilter.cs`; Test `ReferenceCandidateScannerTests.cs`, `OverloadFilterTests.cs`.

**Interfaces — Produces:**
- `ReferenceCandidateScanner.FindConstructorTokens(IList<IToken> tokens, string typeName) : IList<IToken>` — `ID == typeName` followed by `{`, `SUPER`/`SELF` followed by `(`, `CONSTRUCTOR` keyword tokens.
- `OverloadFilter.Arguments` also reads `{...}` after a name; `NameAt` also returns `SUPER`, `SELF`, `CONSTRUCTOR` tokens.

- [ ] Tests: `ConstructorCandidatesAreCreationsChainsAndDeclarations` (input with `Foo{1}`, `AS Foo`, `SUPER(1)`, `SUPER:Bar()`, `SELF(2)`, `SELF:Bar()`, `CONSTRUCTOR(n AS INT)` → texts `Foo`, `SUPER`, `SELF`, `CONSTRUCTOR`); `CurlyArgumentListOfACreation` (`Foo{1, "a"}` → 2 arguments; `Foo{}` → 0); `NameAtFindsSuperAndSelf`.
- [ ] RED, implement, GREEN, commit.

### Task 2: A constructor of the class is a reference to the class

**Files:** Modify `FindReferences/SymbolIdentity.cs`; Test `SymbolIdentityTests.cs`.

**Interfaces — Produces:** `SymbolIdentity.IsReferenceTo(IXSymbol target, IXSymbol candidate) : bool` — `IsSame`, or target is a type and candidate is a constructor whose declaring type (`ParentType`, else `DeclaringType` name) is that type.

- [ ] Tests: `ConstructorRefersToItsClass`, `ConstructorOfOtherClassWithSameNameDoesNot` (other namespace), `MethodDoesNotReferToItsClass`.
- [ ] RED, implement, GREEN, commit.

### Task 3: Engine and command use them

**Files:** Modify `FindReferences/FindReferencesEngine.cs`, `Commands/FindReferencesCommand.cs`, `FindReferences/ReferencesTableDataSource.cs` (title only if it prints `target.Name`); Test `FindReferencesEngineTests.cs`.

**Interfaces — Produces:** `FindReferencesEngine.SearchName(IXSymbol target) : string` (type short name for a constructor, else `Name`); `DeclaredEntity` maps a `CONSTRUCTOR` token to the constructor entity starting on its line.

- [ ] Tests: `SearchNameOfAConstructorIsItsClass`, `DeclaredEntityOfConstructorKeyword` (two classes with constructors in one text; each keyword maps to its own entity, `Foo{}` maps to null).
- [ ] Engine: constructor target → `FindConstructorTokens`, match with `IsReferenceTo`; command: target constructor whose parent type has no declared constructors → the type; window title for a constructor `Foo{}`.
- [ ] RED, implement, GREEN, commit.

### Task 4: Exp verification, report, memory

- [ ] FarTest: add `CONSTRUCTOR()`/`CONSTRUCTOR(n AS INT)` to a class, a subclass with `SUPER(n)`, `SELF(0)`, creations `Foo{}`/`Foo{1}`; run the harness from the declaration, from a creation, and on the class name; check the hit lists.
- [ ] Update the spike report (Known gaps), memory; commit.
