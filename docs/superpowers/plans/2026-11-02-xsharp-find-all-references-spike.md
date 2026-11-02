# Find All References (X# Language Service) Spike Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Shift+F12 on an X# source symbol fills the VS Find All References window with verified hits from open and closed files, off the UI thread, plus measured cost on acslib.

**Architecture:** Reuse the Go To Definition resolver. Per file: text pre-filter → X# lexer → `ID` tokens named like the target → resolve each with `XSharpLookup.RetrieveElement` → keep those identical to the target. Closed files get a scratch `ITextBuffer` + `XDocument` built by the same indexing code the classifier uses. Results stream into `IFindAllReferencesService` as an `ITableDataSource`.

**Tech Stack:** C# (`LanguageService2022.csproj`, net48, VSSDK 17, Community.VisualStudio.Toolkit.17), X# code model (`XSharp.CodeModel`), X# lexer (`XSharp.Parser.VsParser`), xUnit 2.4.2, `msbuild` shim, VS 2022 Exp instance.

**Spec:** `docs/superpowers/specs/2026-11-02-xsharp-find-all-references-spike-design.md` (decisions D1–D10, exit criteria 1–3)

## Global Constraints

- New worktree `C:\Projects\External\XSharpPublic-far` on new branch `feature/find-all-references` from `dev`; copy the spec and this plan there first. Local commits allowed (`git -C <worktree> commit`, explicit paths); no push, merge or PR.
- Fresh worktree: build `src/Compiler.slnx` first (not `buildcompiler.cmd`), then VS projects.
- VS projects always with `-p:Platform=AnyCPU` (not `Any CPU`); single-project builds: `msbuild <proj> -restore -t:Build -p:Configuration=Debug -p:Platform=AnyCPU -bl:<sp>\build.binlog`.
- C# only in this spike, no `.prg` changes in `XSharpCodeModelXs`. No new identifier contains `CSharp`. No xslint.
- Code lives in `src/VisualStudio/LanguageService/FindReferences/` (new folder), namespace `XSharp.LanguageService.FindReferences`. Edit existing files with the Edit tool; CRLF, UTF-8 BOM, 4-space indent.
- `LanguageService.csproj` (VS 2019) must still build: add every new `.cs` to both project files.
- Nothing from acslib or winformsconversiontwo is committed; their logs go to the scratchpad `<sp>`.
- Search work never runs on the UI thread; every `XSharpLookup` call sits between `ModelWalker.Suspend()` and `ModelWalker.Resume()` (as `GetSymbolUnderCursor` does).

## Review Focus

1. A file that is open **with unsaved edits** must be searched in its buffer text, not its disk text (D8). → Task 4 Exp check 4.
2. FAR started while a project is still loading or the `.xsdb` is being rebuilt must end with a partial list and a log line, not an exception or a hang. → Task 4 Exp check 5.
3. A second Shift+F12 before the first search finishes cancels the first; no mixed results in the window. → Task 5 Exp check 3.
4. Identifiers inside `TEXT … ENDTEXT`, `/* */`, `//`, `&&`, `*` comment lines and strings never show up (D3). → Task 1 tests.
5. Same name, different symbol (a method `Close` in two unrelated classes; a local shadowing a field) must not leak into the result. → Task 2 tests + Task 4 Exp check 2.

---

### Task 1: Test project and candidate scanner

**Files:**
- Create: `src/VisualStudio/LanguageService.Tests/LanguageService.Tests.csproj` (SDK-style, `net48`, AssemblyName `XSharp.LanguageService.Tests`, signed with the same `SignAssembly`/`AssemblyOriginatorKeyFile` values the LanguageService gets from `Common.props`/`Shared.props` (copy the properties, do not import the old-style props), xunit 2.4.2 + xunit.runner.visualstudio + Microsoft.NET.Test.Sdk, ProjectReference to `..\LanguageService\LanguageService2022.csproj`)
- Create: `src/VisualStudio/LanguageService/FindReferences/ReferenceCandidateScanner.cs`
- Modify: `src/VisualStudio/LanguageService/Properties/AssemblyInfo.cs` — `[assembly: InternalsVisibleTo("XSharp.LanguageService.Tests, PublicKey=…")]`, key from `sn -Tp` on the built `XSharp.LanguageService2022.dll`
- Modify: `src/VSIntegration2022.sln` — add the test project
- Test: `src/VisualStudio/LanguageService.Tests/ReferenceCandidateScannerTests.cs`

**Interfaces:**
- Produces: `internal static class ReferenceCandidateScanner { static bool MayContain(string text, string name); static IList<IToken> FindIdentifierTokens(IList<IToken> tokens, string name); }` — `MayContain` = `text.IndexOf(name, StringComparison.OrdinalIgnoreCase) >= 0`; `FindIdentifierTokens` keeps `Type == XSharpLexer.ID`, `Channel == XSharpLexer.DefaultTokenChannel`, text equal ordinal-ignore-case. Tests lex with `XSharp.Parser.VsParser.Lex(text, "test.prg", XSharpParseOptions.Default, listener, out stream, out _)`.

- [ ] **Step 1: Write failing tests** `ReferenceCandidateScannerTests`:
  - `FindsIdentifierCaseInsensitive`: `"local oFoo as Foo\noFoo := FOO{}\n"`, name `foo` → 2 tokens (`Foo`, `FOO`), not `oFoo`.
  - `SkipsCommentsAndStrings`: `"// Foo\n/* Foo */\n&& Foo\n? \"Foo\"\nx := Foo\n"` → 1 token on line 5.
  - `SkipsTextEndText`: `"TEXT\nFoo\nENDTEXT\nFoo()\n"` (VO dialect options) → 1 token on line 4.
  - `SkipsInactiveIfdef`: `"#ifdef NOTDEFINED\nFoo()\n#endif\nFoo()\n"` → 1 token on line 4.
  - `MayContainIsCaseInsensitive`: `MayContain("x := FOO", "foo")` is true, `MayContain("x := Bar", "foo")` is false.
- [ ] **Step 2: Build and run, expect FAIL** (`ReferenceCandidateScanner` missing): `msbuild src/VisualStudio/LanguageService.Tests/LanguageService.Tests.csproj -restore -t:Build -p:Configuration=Debug -p:Platform=AnyCPU`, then `dotnet test src/VisualStudio/LanguageService.Tests/LanguageService.Tests.csproj --no-build`.
- [ ] **Step 3: Implement `ReferenceCandidateScanner`** and the `InternalsVisibleTo`; add the file to both LanguageService project files.
- [ ] **Step 4: Run tests, expect 5 PASS.**
- [ ] **Step 5: Commit** `[VS] Find references: candidate scanner and LanguageService test project`.

### Task 2: Symbol resolver and identity

**Files:**
- Create: `src/VisualStudio/LanguageService/FindReferences/SymbolResolver.cs`
- Create: `src/VisualStudio/LanguageService/FindReferences/SymbolIdentity.cs`
- Modify: `src/VisualStudio/LanguageService/Extensions.cs:123-160` — `GetSymbolUnderCursor` keeps its signature and calls `SymbolResolver.ResolveAt`
- Test: `src/VisualStudio/LanguageService.Tests/SymbolIdentityTests.cs`

**Interfaces:**
- Produces: `internal static IList<IXSymbol> SymbolResolver.ResolveAt(XSharpSearchLocation location, out CompletionState state, out IList<IToken> tokenList)` — body moved from `GetSymbolUnderCursor` (token list, `RetrieveElement`, retry with last `ID` token, `Suspend`/`Resume` stay in the caller `GetSymbolUnderCursor`, not here, so the engine can suspend once per search).
- Produces: `internal static bool SymbolIdentity.IsSame(IXSymbol target, IXSymbol candidate)` per D5; `internal static bool SymbolIdentity.IsPropertyLike(Kind k)` true for `Kind.Property`, `Kind.Access`, `Kind.Assign`.

- [ ] **Step 1: Write failing tests** `SymbolIdentityTests` (build symbols with the code-model constructors, `new XFile(path, null)`):
  - `SameDeclarationIsSame`: same file, same `Interval.Start` → true.
  - `SameNameOtherTypeIsNotSame`: method `Close` in `ClassA` vs `Close` in `ClassB` → false.
  - `AccessAndAssignAreSame`: `Kind.Access` `Name` and `Kind.Assign` `NAME` in the same type → true.
  - `PartialTypesAreSame`: two `XSourceTypeSymbol` `Ns.Foo` / `ns.foo` in different files → true.
  - `LocalShadowingFieldIsNotSame`: `XSourceVariableSymbol` `nCount` vs field `nCount` → false.
- [ ] **Step 2: Run, expect FAIL** (same commands as Task 1).
- [ ] **Step 3: Implement `SymbolIdentity`, move resolver code into `SymbolResolver.ResolveAt`.**
- [ ] **Step 4: Run tests, expect PASS (10 total).**
- [ ] **Step 5: Exp regression check:** build `ProjectPackage2022` (deploys to the Exp hive), open a test solution, F12 on a type, method, local, system type → same targets as before the change.
- [ ] **Step 6: Commit** `[VS] Find references: extract SymbolResolver, add SymbolIdentity`.

### Task 3: Document index for closed files

**Files:**
- Create: `src/VisualStudio/LanguageService/FindReferences/XDocumentIndexer.cs`
- Create: `src/VisualStudio/LanguageService/FindReferences/ReferenceDocumentSource.cs`
- Modify: `src/VisualStudio/LanguageService/Classifier/XSharpClassifier.cs` (~lines 731–780, the `lineTokens`/`ids` loop) — call `XDocumentIndexer.BuildIndexes` instead of the inline loop
- Test: `src/VisualStudio/LanguageService.Tests/XDocumentIndexerTests.cs`

**Interfaces:**
- Produces: `internal static void XDocumentIndexer.BuildIndexes(IList<IToken> tokens, int lineCount, out Dictionary<int, IList<IToken>> lineTokens, out ConcurrentDictionary<string, IList<IToken>> identifiers)` — exactly the classifier's current rules (moved, not rewritten).
- Produces: `internal sealed class ReferenceDocumentSource` with ctor `(ITextBufferFactoryService bufferFactory, IContentTypeRegistryService contentTypes, IReadOnlyDictionary<string, ITextBuffer> openBuffers)` and `internal XDocument GetDocument(XFile file)` — open buffer (path key, ignore case) → its `GetDocument()` if the snapshot version matches, else lex its current snapshot; closed → `File.ReadAllText`, scratch buffer with content type `text`, `XFile` stored in `buffer.Properties`, `new SourceWalker(file, false).Lex(text)`, `new XDocument(buffer, tokens, snapshot)` + `SetTokens`/`SetIdentifiers` from `BuildIndexes`, `buffer.Properties.AddProperty(typeof(XDocument), doc)` so `location.GetDocument()` finds it.
- `openBuffers` is collected once on the UI thread at search start from the running document table (`IVsEditorAdaptersFactoryService.GetDocumentBuffer` of each X# `IVsTextLines`).

- [ ] **Step 1: Write failing tests** `XDocumentIndexerTests`: `"FUNCTION Foo()\nlocal x\nx := Foo()\n"` → `lineTokens` has keys 0,1,2; `identifiers["FOO"]` has 2 tokens (case-insensitive key); comment-only line has no `identifiers` entry.
- [ ] **Step 2: Run, expect FAIL.**
- [ ] **Step 3: Implement `XDocumentIndexer` by moving the classifier loop; implement `ReferenceDocumentSource`.**
- [ ] **Step 4: Run tests, expect PASS.**
- [ ] **Step 5: Exp regression check:** colours, identifier highlighting and Ctrl+K,D formatting in an open `.prg` unchanged.
- [ ] **Step 6: Commit** `[VS] Find references: shared XDocument indexer, documents for closed files`.

### Task 4: Search engine

**Files:**
- Create: `src/VisualStudio/LanguageService/FindReferences/FindReferencesEngine.cs`
- Create: `src/VisualStudio/LanguageService/FindReferences/ReferenceHit.cs`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: `internal sealed class ReferenceHit { string FilePath; string ProjectName; int Line /*0-based*/; int Column /*0-based*/; int Length; string LineText; bool IsDefinition; }`
- Produces: `internal interface IReferenceSink { void Add(IReadOnlyList<ReferenceHit> batch); }`
- Produces: `internal sealed class FindReferencesStats { int FilesInScope, FilesLexed, Candidates, Resolved, Unresolved, Hits; TimeSpan Elapsed; }`
- Produces: `internal Task<FindReferencesStats> FindReferencesEngine.FindAsync(IXSymbol target, XSharpSearchLocation origin, ReferenceDocumentSource documents, IReferenceSink sink, CancellationToken ct)` — runs on `TaskScheduler.Default`; scope per D2 (`XSolution.Projects` filtered by `ReferencedProjects`; locals/parameters: origin member's interval only); per file `MayContain` → `GetDocument` → `FindIdentifierTokens` → for each token a location `new XSharpSearchLocation(doc, file, XSharpLookup.FindMember(line, file), snapshot, line, token.StartIndex, ns)` → `SymbolResolver.ResolveAt` → `SymbolIdentity.IsSame`; `Foo{}` handled through the type match (D7); declarations of the target (all partial parts) added with `IsDefinition = true`; one `Add` per file; `ct` checked per file and per token; stats line written with `Logger.Information` ("FindReferences …").

- [ ] **Step 1: Implement engine and hit types** (no unit test: needs a loaded solution; covered by the Exp checks below).
- [ ] **Step 2: Temporary debug hook:** create `Commands/FindReferencesCommand.cs` (Task 5 file) intercepting `VSStd97CmdID.FindReferences` with a sink that writes hits to the X# intellisense log; Task 5 replaces the sink with the FAR window.
- [ ] **Step 3: Exp checks** on a test solution in `<sp>\FarTest\` (two projects, B references A; A has `CLASS Foo` partial over two files, `ACCESS/ASSIGN Name`, method `Close` also in an unrelated `CLASS Bar`, `FUNCTION Helper`, a local shadowing a field, comment/string occurrences, one closed file using `Foo`):
  1. FAR on `Foo`, `Name`, `Close` (Foo's), `Helper`, the local → log hit list equals the expected list written in `<sp>\FarTest\expected.txt` before the run.
  2. `Bar:Close` calls absent from `Foo:Close` results.
  3. Hits in the closed file present.
  4. Edit an open file without saving (add `Helper()`), FAR on `Helper` → the unsaved line is a hit.
  5. Start FAR right after opening the solution (projects loading) → partial result + log line, no exception.
- [ ] **Step 4: Commit** `[VS] Find references: search engine`.

### Task 5: FAR window, navigation, command

**Files:**
- Create: `src/VisualStudio/LanguageService/FindReferences/ReferencesTableDataSource.cs` (`ITableDataSource` + `ITableEntriesSnapshotFactory`, `SourceTypeIdentifier = "XSharpFindReferences"`, implements `IReferenceSink`)
- Create: `src/VisualStudio/LanguageService/FindReferences/ReferencesTableNavigator.cs` (`[Export(typeof(ITableControlEventProcessorProvider))]`, `[DataSourceType("XSharpFindReferences")]`, `[DataSource("XSharpFindReferences")]`, `[Name("XSharpFindReferencesNavigator")]`, `[Order(Before = "Default")]`; `PreprocessNavigate` opens the file and sets the caret, `e.Handled = true`)
- Modify: `src/VisualStudio/LanguageService/Commands/FindReferencesCommand.cs` (created in Task 4; pattern of `GotoCommand.cs`: `InitializeAsync` intercepts `VSStd97CmdID.FindReferences`)
- Modify: where `GotoCommand.InitializeAsync()` is called — also call `FindReferencesCommand.InitializeAsync()`

**Interfaces:**
- Consumes: `FindReferencesEngine.FindAsync`, `IReferenceSink`, `ReferenceHit`.
- Columns per D10: `StandardTableKeyNames.Text` (`LineText` trimmed), `DocumentName`, `Line`, `Column`, `ProjectName`; definitions grouped first via `StandardTableKeyNames2.Definition` text "Definition".
- Command flow: on the UI thread `GetSymbolUnderCursor` → no symbol: status bar "No X# symbol under the cursor" and return; else `IFindAllReferencesService.StartSearch($"'{target.Name}' references")`, add the data source to `window.Manager`, collect open buffers, start `FindAsync` with a `CancellationTokenSource` held in a static field — a new search cancels the previous one.

- [ ] **Step 1: Implement the three classes and the registration; remove the Task 4 log sink.**
- [ ] **Step 2: Exp checks:**
  1. Task 4 check 1 cases appear in the FAR window, grouped by project/file, definitions marked.
  2. Double-click on a hit in a closed file opens it at the right line and column.
  3. Start FAR on `Foo`, immediately FAR on `Helper` → window shows only `Helper` results.
  4. While FAR runs on a big project, typing in another `.prg` does not stall (UI responsive).
  5. Shift+F12 in a C# file of a mixed solution still runs the C# FAR (command not swallowed for non-X# views).
- [ ] **Step 3: Build `LanguageService.csproj` (VS 2019) to confirm it still compiles.**
- [ ] **Step 4: Commit** `[VS] Find references: results in the Find All References window`.

### Task 6: Measurement and spike report

**Files:**
- Create: `docs/superpowers/specs/2026-11-02-xsharp-find-all-references-spike-report.md`

- [ ] **Step 1:** Open acslib in the Exp instance (wait until the code model DB is complete; check the intellisense log). Run FAR three times each on: a rare class name, a mid-frequency method, `Init`, `Close`. Record the `FindReferences` stats lines (median of 3) into `<sp>\far-measure.tsv`.
- [ ] **Step 2:** Spot-check 20 `Unresolved` tokens for `Close`: how many are late-bound sends (D6) vs resolver gaps.
- [ ] **Step 3:** Write the report: table of the stats, the spot-check result, and a recommendation each for D4 (index in `.xsdb` yes/no, with the threshold that triggered it) and D6 (show possible references yes/no). List known gaps (D7 constructors, PE symbols) as follow-ups.
- [ ] **Step 4: Commit** `[Docs] Find references spike report`.
