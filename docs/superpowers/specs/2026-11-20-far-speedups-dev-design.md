# Find All References speed-ups on dev — design

Date: 2026-11-20 · Branch: `hpetriffer/far-speedups-dev` (base `origin/dev`) · Size: small change (port)

## Intent

Find All References took 49 s to 7 minutes per symbol on a 39,000-file solution (RadixWf). Three changes measured on
the parser branch (`feature/parser-phase1-diff`, not merged) cut that to 3.5–68 s with identical hits: class
`SqlSelectBase` 142 → 37 s, method `Execute` 418 → 68 s, property `SqlString` 255 → 54 s, method `GetData` 49 → 3.5 s.
The code they touch is the same on dev, so they are ported here on their own.

## What is ported

1. **Whole-word prefilter** — `ReferenceCandidateScanner.MayContain` accepts a file only when the name occurs as a whole
   word (uses the existing `IsWordAt`). A file with only `GetDataSet` is no longer lexed for `GetData`. Every hit the
   search can report (identifier, `#symbol`, interpolation hole, word in a comment or string) is a whole word already.
2. **Type cache per search** — `XProject.GetType(IList<XDbResult>)` rebuilt the type from the database (source text +
   parse) for every candidate. New `XTypeCacheScope` (thread static, nestable, `IDisposable`) keeps the built types,
   keyed by the looking project and the database rows. `FindReferencesEngine.Find` opens the scope around the search and
   disposes it at the end, so no cached type outlives the search. Other threads (completion, QuickInfo, the
   ModelWalker) never see it.
3. **Extension cache per search** — `XProject.GetExtensions(TypeName)` searched every assembly reference (including a
   disk check per assembly, `XAssembly.Refresh`) for every candidate. Inside the scope the result is kept per project
   and type name; each caller gets its own copy of the list because callers may change it.

## What is not ported

- The `Prepare`/`PreparedFile` split of `SearchFile`: it depends on the parser branch's token bridge
  (`ReferenceDocumentSource.Lex(file, text, out parse)`), which dev does not have.
- The read-ahead readers of the search (removed again on the parser branch: noise, no gain).
- The phase timings (`FindReferencesStats` phase ticks, `XProject.DbTypeBuilds/DbTypeBuildTicks`, the
  `FindReferences phases ...` log line): used for the before/after measurement only, kept out of dev.

## Trade-offs

- During one search the code model does not see a type or an assembly that changes in the middle of it. A search is a
  snapshot anyway (the ModelWalker is suspended while it runs); the next search builds everything again.
- Memory: the cache holds the types and extension lists one search touches, and is released at the end of the search.

## Relation to `hpetriffer/xassembly-refresh-throttle`

That branch (not pushed) reuses the disk check of each `XAssembly` for 5 s and clears a project's assembly type cache
after any read of its assemblies (`RefreshAssemblyReferences`). It works everywhere in the IDE, and it removes most
of the disk time that change 3 saves. Change 3 still saves the extension search itself (walking every reference per
candidate). The two branches touch different methods of `XProject.prg` (`RefreshAssemblyReferences` there, `GetType`
and `GetExtensions` here) and merge in either order without conflicts. Measured on top of the throttle fix, so the
numbers show what this branch adds to it.

## Verification

- Unit tests: `ReferenceCandidateScannerTests.MayContainNeedsAWholeWord`, `TypeCacheScopeTests` (scope, nesting,
  thread isolation, key, extension copies).
- VS measurement: RadixWf copy in the Exp instance, alternating runs in one session with the caches switched on and
  off (switch only in the local measurement build).
