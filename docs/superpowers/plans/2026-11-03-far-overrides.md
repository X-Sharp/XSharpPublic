# Find All References: Overrides — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** FAR on a method/property lists its whole override family, one window group per family member.

**Architecture:** New `FindReferences/OverrideFamily.cs` finds the topmost member of a family through the `BaseType`
chain and decides membership; the engine accepts family members as definitions and references and tags each hit with
its group; the table data source keeps one `DefinitionBucket` per group; the scope starts at the family root.

**Tech Stack:** C# (VS 2022 language service), xUnit (`LanguageService.Tests`, references the built DLL).

**Spec:** `docs/superpowers/specs/2026-11-03-far-overrides-design.md`

## Global Constraints

- `VIRTUAL`/`OVERRIDE` ignored; property-like members match by name; no interfaces, no PE types, no late-bound sends.
- CRLF, UTF-8 BOM, Edit tool; build `LanguageService2022.csproj` before the tests.

## Review Focus

- A base type that cannot be resolved (null) or a cyclic `INHERIT` must end the walk — pinned in Task 1.
- Same-named methods with another parameter list in a subclass are overloads, not overrides — Task 1.
- Two unrelated classes with `Close` stay separate — Task 1.

---

### Task 1: OverrideFamily

**Files:** Create `FindReferences/OverrideFamily.cs`; Test `LanguageService.Tests/OverrideFamilyTests.cs`.

**Interfaces — Produces:**
- `OverrideFamily.Root(IXMemberSymbol member, Func<IXTypeSymbol, IXTypeSymbol> baseOf = null) : IXMemberSymbol` — topmost member with the same name/kind/signature up the chain (`baseOf` defaults to `t => t.BaseType`); the member itself when none.
- `OverrideFamily.Contains(IXSymbol target, IXSymbol candidate, Func<IXTypeSymbol, IXTypeSymbol> baseOf = null) : bool`.

- [ ] Tests: `OverrideBelongsToTheBaseMembersFamily` (both directions), `SiblingOverridesShareTheFamily`, `OverloadInSubclassIsNotFamily`, `UnrelatedClassesAreSeparate`, `PropertyLikeMatchByName`, `CyclicOrMissingBaseEndsTheWalk`.
- [ ] RED, implement, GREEN, commit.

### Task 2: Engine, groups, scope

**Files:** Modify `FindReferencesEngine.cs`, `ReferenceHit.cs` (`string Group`), `ReferencesTableDataSource.cs` (bucket per group); Test `FindReferencesEngineTests.cs`.

**Interfaces — Produces:** `FindReferencesEngine.GroupOf(IXSymbol symbol) : string` = `"{KindKeyword} {FullName}"` trimmed.

- [ ] Test `GroupOfAMemberNamesKindAndFullName`.
- [ ] Engine: definition if declared entity is target or in family; reference if `IsReferenceTo` or family; `Group` = group of the matched/declared member; scope from `Root(target)`.
- [ ] RED, implement, GREEN, commit.

### Task 3: Exp verification, report, memory

- [ ] FarTest: `Base.Close`, `Sub INHERIT Base` overriding `Close`, `Sub2 INHERIT Base` overriding `Close`, calls on each type; FAR from `Base.Close`, `Sub.Close`, a call `oSub:Close()`; check groups via the log.
- [ ] Report, memory, commit.
