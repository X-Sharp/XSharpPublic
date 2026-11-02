# Find All References for the X# Language Service — Spike Design

Date: 2026-11-02 · Status: draft for review

## Problem

The X# language service (`src/VisualStudio/LanguageService`) has Go To Definition and Peek, but no
Find All References (Shift+F12). Nothing intercepts `VSStd97CmdID.FindReferences`; the old
`Library` code (Class View/Object Browser search) is compiled out (`#if LIBRARYMANAGER`).

## What exists and is reused

- **Symbol under the cursor:** `ITextView.GetSymbolUnderCursor` (`LanguageService/Extensions.cs:123`)
  → `XSharpTokenTools.GetTokensUnderCursor` → `XSharpLookup.RetrieveElement(location, tokens, state)`.
- **Search location:** `XSharpSearchLocation(XDocument, XFile, member, ITextSnapshot, line, position, ns)`.
  It needs an `XDocument` (tokens per line + identifier index) and a snapshot — today only the
  classifier of an *open* buffer builds one (`XSharpClassifier.cs`, ~lines 720–850).
- **Code model DB (`.xsdb`):** declarations only (`Types`, `Members`, `Files`, `FilesPerProject`).
  No usage table.
- **FAR window:** `IFindAllReferencesService` / `SVsFindAllReferences` in
  `Microsoft.VisualStudio.Shell.Framework` (already referenced, VS 2019 and 2022). Results are an
  `ITableDataSource`.

## Spike goal

Shift+F12 on a source symbol of the open solution fills the VS Find All References window with
correct hits, from open and closed files, without blocking the UI thread. The spike also measures
cost on a large VO code base (acslib) to decide whether a usage index in the `.xsdb` is needed.

## Decisions (spike defaults; revisit after measurement)

| # | Topic | Spike decision |
|---|---|---|
| D1 | Symbols supported | Source-declared types, members (method, property incl. ACCESS/ASSIGN, field, event), functions/procedures, globals, defines; locals and parameters within their member. PE symbols from referenced assemblies: out of scope. |
| D2 | Search scope | The target's project plus every `XProject` whose `ReferencedProjects` contains it; locals/parameters: the containing member only. |
| D3 | Candidate filter | Text pre-filter (`IndexOf(name, OrdinalIgnoreCase)`) per file, then X# lexer; only `ID` tokens on the default channel whose text equals the name (ordinal ignore case). Comments, strings, inactive `#ifdef` code never match. |
| D4 | Verification | Every candidate is resolved with the Go To Definition resolver and compared to the target (D5). No index. |
| D5 | Identity | Source symbols match when declared in the same file at the same start offset. Types also match by `FullName` (ignore case) so partial classes count. ACCESS, ASSIGN and PROPERTY of the same name in the same type are one symbol. |
| D6 | Unresolved hits | Name matches that resolve to nothing (late binding on `USUAL`/`OBJECT`, `:` sends) are not shown; their count is logged. Showing them as "possible references" is decided after measurement. |
| D7 | Constructors | `Foo{}` counts as a reference to type `Foo`. FAR on `Init`/`CONSTRUCTOR` lists declarations only (known gap). |
| D8 | Document source | Open file → its current buffer snapshot. Closed file → disk text in a scratch `ITextBuffer` (content type `text`), lexed with `SourceWalker.Lex`. |
| D9 | Threading | Search runs on the thread pool between `ModelWalker.Suspend()` / `Resume()`, cancellable; results stream into the window in batches. |
| D10 | UI | VS FAR window, columns: code line, file, line, column, project; declaration rows marked as definitions. Double-click navigates. |

## Out of scope

Rename, CodeLens, usage index in the `.xsdb`, PE symbols, VS 2019 verification (must still build).

## Exit criteria

1. FAR on a type, a method, an ACCESS/ASSIGN property, a function, a local: hit lists match a manual
   `rg` count minus comments/strings/unrelated same-name symbols, on a test solution.
2. UI thread stays responsive during the search (typing in another file while it runs).
3. Measurement on acslib for a rare name and for `Init`/`Close`: wall time, files lexed,
   candidates, resolved, unresolved — written to the spike report with a recommendation on D4/D6.
