# Find All References for the X# Language Service — Spike Report

Date: 2026-11-02 · Branch: `feature/find-all-references` (local, not pushed) · Design: `2026-11-02-xsharp-find-all-references-spike-design.md`

## Result

Shift+F12 on an X# source symbol fills the VS Find All References window. Hits come from open files (current buffer
text, unsaved edits included) and closed files; the search runs on the thread pool and a new search cancels the
running one. No usage index is needed at acslib scale.

## Exit criteria

| # | Criterion | Status |
|---|---|---|
| 1 | Hit lists on the test solution (type, method, ACCESS/ASSIGN, function, local) equal the expected list | MET — all five cases equal `expected.txt`, including the partial class, `Bar:Close` excluded, a never-opened file, an unsaved edit, a local shadowing a field |
| 2 | UI thread responsive during a search | MET — DTE round trip during searches on acslib ≤ 148 ms (9 searches) |
| 3 | Measurement on acslib with recommendation for D4/D6 | MET — see below |

## Measurement (acslib, 31 projects, 4166 `.prg`; VS 2022 Exp, warm `.xsdb`; median of 3)

| Target | Files in scope | Files lexed | Candidates | Resolved | Unresolved | Hits | Median |
|---|---:|---:|---:|---:|---:|---:|---:|
| rare class (8 word occurrences) | 3537 | 7 | 3 | 2 | 0 | 3 | 0.65 s |
| method, 77 word occurrences | 2472 | 8 | 20 | 19 | 0 | 20 | 3.8 s |
| `Close` of one class (1224 word occurrences) | 775 | 164 | 327 | 274 | 52 | 1 | 7.4 s |

Runs: 654/1134/557 ms, 3814/4372/3651 ms, 7723/7444/7276 ms.

Observations:
- The text pre-filter is cheap: 3537 files read and filtered in about 0.6 s.
- Time grows with *resolved candidates*, not with files: about 25 ms per resolution for `Close`, more for the
  method case (19 resolutions, 3.8 s), where each resolution walks a deeper type hierarchy.
- `Init` was not measured: acslib declares no `METHOD Init`; its occurrences are constructor and `SUPER:Init` calls,
  and constructors are out of scope (D7).

Spot check of 20 unresolved `Close` tokens: 18 are sends on variables declared `AS OBJECT` (late bound, not
resolvable by design), 1 is a `VAR` initialised from a method call (possible type-inference gap), 1 is a field
declared outside the file (base or partial class). The 273 resolved non-matches are `Close` methods of other classes
and are correctly excluded.

## Recommendations

- **D4 (index): no `.xsdb` usage index now.** Worst case 7.4 s for a very common name, UI responsive, results stream
  in. The cost is in symbol resolution, so a per-search cache of type lookups (same type name resolved once per file)
  is the cheaper next step. An index pays off only together with Rename/CodeLens.
- **D6 (late-bound sends): show them, in their own group.** In VO code the `AS OBJECT` sends are the calls a user is
  looking for (18 of 20 sampled). Proposal: a second definition bucket "Possible references (late bound)" with the
  unresolved name matches whose receiver type is `OBJECT`/`USUAL`, off by default or clearly labelled.
- **Follow-up: F12 with several definitions** should use the same FAR window instead of the modal
  `GotoDefinitionResultsWindow` (C# behaviour; asked by the user during the spike). `ReferencesTableDataSource`
  can carry the definitions as is.

## Findings outside the plan

- `XProject.FindXFile` creates a new `XFile` per call, without entities.
- `XFile.Parse()` writes the `.xsdb` and then clears the entities of every file that is not open
  (`SaveToDatabase` → `Clear`). `XSharpLookup.FindMember(line, file)` falls back to `file.Parse()`, so any lookup
  on a closed file writes the database. The spike parses with `new SourceWalker(file, false)` instead (no write).
- Members resolved from the database have `File == null`; identity falls back to kind + name + declaring type +
  parameter list.
- F12 on `oFoo:Close()` / `oFoo:Name` did not move the caret in the DTE harness, with and without the spike
  changes (not investigated; FAR resolves the same expressions).

## Known gaps

- Navigation from the FAR window (double click) was verified manually on 2026-11-03 (test solution, closed file
  `Bar.prg`). Each double click logs `navigate to` twice; not investigated.
- The Code column (`linetext`, not `text`) shows the line in the editor colors with the hit in the reference
  highlight, like C#. The colors come from a token-type mapping that follows the classifier's lexer rules without its
  extra keywords and its bracketed-string rule.
- FAR while projects are still loading was only tried on the 2-project test solution, where loading had finished.
- Overloads: `XSharpLookup.SearchMethod` returns every overload of a name. Since 2026-11-03 FAR keeps only the
  overloads whose parameter count accepts the number of arguments at the call site (`OverloadFilter`; defaults,
  PARAMS arrays, CLIPPER methods and extension methods considered), both for the hits and for the target picked at a
  call site. Verified in the Exp instance with three `Run` overloads, also with the declaring file closed (members
  from the database carry their parameters). When no overload accepts the count, all are kept. Go To Definition is
  unchanged.
- Overloads with the same count are narrowed by argument type (2026-11-03) where the type is obvious from the tokens:
  literals, `Foo{...}`, and single identifiers typed by the lookup (locals, parameters, fields, properties). Only
  built-in types of different kinds (string vs. number vs. date …) rule an overload out; integers widen to floating
  point, USUAL/OBJECT accept anything, class types are never ruled out (inheritance unknown); among the rest the most
  exact matches win. Expressions and method calls as arguments stay unknown, so `Run((1 + 2))` is listed for both
  `Run(INT)` and `Run(STRING)`. Declaration names are no longer resolved as calls (their parameter list was read as
  arguments). Verified in the Exp instance with `Run()`, `Run(INT)`, `Run(STRING)`, `Run(INT, INT)`.
- The first search right after adding files to a project left calls unresolved (the code model had not caught up);
  the same search a minute later was complete.
- Overrides (2026-11-03, design `2026-11-03-far-overrides-design.md`): FAR on a method or property lists its whole
  override family as in C#, both directions, siblings included, one window group per declaration. Same name, kind
  and parameter list count as an override (`VIRTUAL`/`OVERRIDE` not checked); property-like members by name; only
  source types; the scope starts at the topmost member. `XSourceTypeSymbol.BaseType` is only set by `ForceComplete`
  and that looks the base class up with the file's USINGs alone (not the enclosing `BEGIN NAMESPACE`), so FAR
  resolves the base class itself. Verified in the Exp instance: `Animal`/`Dog`/`Cat` `Speak` family from three start
  points, an overload `Speak(n)` and an unrelated `Car.Speak` stay separate; round trips ≤ 136 ms.
- Constructors (2026-11-03, design `2026-11-03-far-constructors-design.md`): FAR on `CONSTRUCTOR(...)`, on `Foo{...}`,
  `SUPER(...)` or `SELF(...)` lists the creations, the chains in derived classes and in the same class, and the
  declaration; overloads are narrowed by the `{...}`/`(...)` arguments. FAR on a class also lists `Foo{...}`; on
  `Foo{}` of a class without constructors it searches the class. The lookup resolves the name in `Foo{...}` to the
  class (as Go To Definition does), so FAR maps it to the constructors itself. Verified in the Exp instance with two
  constructors, a subclass `SUPER(r)` and `SELF(0)` across two projects (one DTE round trip of 1.17 s during the
  first constructor search, the others ≤ 82 ms). Not covered: `METHOD Init`/`SUPER:Init()` with `/vo1` (user: not
  needed), static constructors.
- Symbols from referenced assemblies, VS 2019 runtime (it builds).
- The per-hit and navigation log lines (spike aid) were removed on 2026-11-03; one summary line per search stays.
  FAR on a symbol from a referenced assembly shows a status bar message instead of an empty window, and a new search
  removes the previous one's source from the window.
- Not addressed (spike review minors): `ModelWalker.Suspend/Resume` count with `++`/`--`, not `Interlocked` (existing
  code model, now also called from the search thread); a running search is not cancelled when the solution closes;
  right after an edit above a declaration its interval can be stale and hide hits in that file; DEFINEs in `.vh`
  headers get no definition row; identifiers inside interpolated strings and `@@`-escaped names are never candidates.
- `XDocumentIndexer` duplicates three index rules of `XSharpClassifier` (kept separate on purpose; may drift).
