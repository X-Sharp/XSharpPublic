# VFPXPorter Settings Reference

The **Exporter Settings** dialog (`SettingsDialog.prg`) is organized into three tabs:
**Folders**, **Export**, and **Project**. The per-export choices of the **Export Project**
window are described at the end. Every checkbox/field is backed by a property on
`ExporterSettings` (which persists it to the `.ini`-as-JSON settings file) and is copied at
export time onto `XPorterSettings` (`ExporterSettings:ToXPorterSettings()`), the object the
export engine actually reads from.

Settings are only applied when you click **Ok**; **Cancel** discards changes made in the
dialog. **Reset Settings** restores all values (via `ExporterSettings:Reset()`) and
re-arms the startup warning; **Open Settings Folder** opens the folder containing the
settings file in Explorer.

---

## Folders tab

| Control | Setting | Default | Effect on export |
|---|---|---|---|
| Default VFP Items Folder | `Items` → `ItemsPath` | *(empty)* | Default source folder browsed to when picking the VFP project (`.pjx`) or items to export. Doesn't affect the export itself, just the dialog's starting folder. |
| Default Output Folder | `Output` → `OutputPath` | *(empty)* | Default destination folder proposed for generated output. See **"Export folder layout"** in `README.md` for how the final output path is built from this plus the project/solution name and *Place solution and project in the same directory* (Project tab). |
| Default XPorter ressources Folder | `RessourcesFolder` → `XPorterSettings.DataFolder` (static) | `<exe folder>\Data` | Folder containing the exporter's own data files: `PropRules.json`, `EventRules.json`, `Statements.json`, `TypeConvert.json`, `ColorProperties.json`, `AliasTypeCollisions.json`, and the `Templates\` tree (`Form`, `Designer`, `Menu`, `SingleFile`, `Others`, `ReportListener`). Changing this repoints the entire rule/template set used to drive conversion — only change it if you maintain a customized copy of these files. |

---

## Export tab

| Control | Setting | Default | Effect on export |
|---|---|---|---|
| Prefix Class filenames with the name of the Library | `PrefixClassFile` | Unchecked (`FALSE`) | When exporting library (VCX) classes, the generated file is named `<VCXFileName>_<ClassName>.prg` instead of just `<ClassName>.prg`, avoiding filename collisions between same-named classes defined in different VCX libraries (`XPorterCtrlForm.prg`). |
| Default Fields modifiers (combo: PRIVATE/PROTECTED/INTERNAL/PUBLIC) | `Modifier` | `PUBLIC` | The chosen keyword is emitted as-is in front of every generated field/property/data-environment declaration line. There's no validation — whatever string is stored here appears literally in the generated source (`XPorterCtrlForm.prg`). |
| Class name prefix | `ClassNamePrefix` (static `XPorterSettings.ClassNamePrefix`) | *(empty)* | Prepended to **every** generated class name (forms, libraries, FormSet sub-forms, Pages, menu container, …) to avoid collisions between VFP class names and cursor aliases sharing the same name (e.g. a class and a work area both named `Customers`). Also emitted as `#define VFPX_CLASS_PREFIX` in `VFPXPorter.xh` for use by `DO FORM`/`CreateObject` UDCs at runtime. Empty means no prefix, fully backward-compatible. |
| **Code Conversion** group | | | |
| Convert VFP Elements to WinForms (This⇒thisObject, Parent⇒_Parent, …) | `ConvertThisObject` | Checked (`TRUE`) | Applies the ThisObject code converter to every method body, rewriting VFP self/parent-style references to their WinForms equivalents. |
| Convert Statement to Call: Release ⇒ Release(), … | `ConvertStatement` | Checked (`TRUE`) | Adds `()` to the method calls listed in `Statements.json` (`.Refresh`, `.Release`, `.Click`, `.SetFocus`, `.Hide`, `NODEFAULT`, `DODEFAULT`) when VFP calls them without parentheses, e.g. `luResult = this.Refresh` ⇒ `this.Refresh()`. Applied by `CodeConverter` to form/class code and loose `.prg` files; menu code always gets it. |
| &nbsp;&nbsp;↳ Convert only if last item on line | `ConvertStatementOnlyIfLast` | Checked (`TRUE`) | Restricts the above conversion to only the last statement of a `;`-continued/multi-statement line, rather than every matching occurrence on the line. |
| Fullname for User-defined Methods | `NameUDF` | Unchecked (`FALSE`) | Unchecked: user-defined methods keep their original short VFP name. Checked: they get the same fully-qualified/built name used for regular event handlers. |
| Keep original code in Comment | `KeepOriginal` | Checked (`TRUE`) | Inserts the original VFP source line as a comment immediately above any line the converter modified — the main "show your work" safety net for reviewing conversions. |
| Remove "Set_" prefix if present in EventRules | `RemoveSet` | Checked (`TRUE`) | Only affects events that could **not** be matched via `EventRules.json` (the unmapped/fallback path in `SCXVCXItem.prg`): if the resulting handler name starts with `Set_`, that prefix is stripped. Mapped events are unaffected — their naming comes entirely from `EventRules.json`. |
| Prefix Event methods with FORM name | `PrefixEvent` | Checked (`TRUE`) | For control-owned events, the owner's name is always prefixed. For **form-level** events, the FORM name prefix (e.g. `Form1_Load`) is only added when this is checked; unchecked, form-level handlers keep the bare event name (e.g. `Load`). |
| Keep Visual FoxPro Event name in EventHandlers | `KeepFoxProEventName` | Checked (`TRUE`) | For events that *are* handled/mapped, controls whether the generated handler uses the raw VFP event name or the converted .NET event name/type. Has no effect on events skipped entirely by *Generate only EventHandlers that have a definition*. |
| Generate only EventHandlers that have a definition | `GenerateOnlyHandledEvent` | Checked (`TRUE`) | Skips generating an EventHandler entirely for any event not found in `EventRules.json`, instead logging it as ignored. This is evaluated before `KeepFoxProEventName`, so unmapped events produce no handler regardless of that setting. |
| Type ThisFormSet with the real FormSet class (instead of Object) | `TypedThisFormSet` | Unchecked (`FALSE`) | For a Form that belongs to a FormSet, VFPXPorter always shadows the inherited `ThisFormSet` with a `NEW` override. Unchecked: typed as `OBJECT` (late-bound, like `ThisForm`). Checked: typed with the FormSet's actual generated class name, giving compile-time member checking at the cost of needing the FormSet class to exist/compile first. |
| Expand WITH...ENDWITH (Workaround for nested WITH) | `ExpandWithEndWith` | Checked (`TRUE`) | Replaces dotted member references inside `WITH` blocks with fully-qualified object paths, working around X# compiler issues with nested `WITH` blocks. The original `WITH`/`ENDWITH` lines are kept as comments. |
| **Edit** button | *(none — opens `Statements.json` in Notepad)* | — | Lets you edit the VFP-statement-to-method-call conversion table used by *Convert Statement to Call* directly. |

---

## Project tab

| Control | Setting | Default | Effect on export |
|---|---|---|---|
| Ignore export Errors | `IgnoreErrors` | Checked (`TRUE`) | When an item fails to export, the exporter logs the error and keeps going with the remaining items instead of stopping the whole export. |
| Keep the original VFP Project Folder Structure | `KeepFolderStructure` | Checked (`TRUE`) | Master switch. When checked, each item is written to a subfolder of the output that mirrors its original location relative to the VFP project's home directory, and *Store Items in Folders by Items type* / *Each ClassLibrary is in a SubFolder* / the folder-name mapping are ignored (and disabled in the UI). Class libraries always get their own subfolder named after the library file regardless (several VCXs commonly share one physical folder, e.g. a project-wide "LIBS" folder, and without this same-named classes in different libraries would overwrite each other); a library that resolves outside the project's home directory entirely (an external dependency from a different VFP project) is placed under a flat `Libs\<LibName>` folder instead of mirroring its unrelated original location. Unchecked: *Store Items in Folders by Items type* / *Each ClassLibrary is in a SubFolder* apply instead, exactly as before this setting was introduced. |
| Store Items in Folders by Items type | `StoreInFolders` | Checked (`TRUE`) | Only used when *Keep the original VFP Project Folder Structure* is unchecked. When checked, each artifact type (Forms, Libs, Menus, Code, Databases, FreeTables, Others) is written into its own subfolder of the output, named per the mapping below. Unchecked: everything is written flat into the output root. |
| &nbsp;&nbsp;↳ **>>`/`<<** button + folder list | `ItemsType` | `Forms=Forms;Libs=Libs;Menus=Menus;Code=Code;Databases=Databases;FreeTables=FreeTables;Others=Others` | Reveals an editable list mapping each of the 7 fixed item types to a physical folder name. Only used when *Store Items in Folders* is checked. If the stored string doesn't parse into exactly 7 segments, it silently falls back to `XPorterSettings.DefaultFolders` (the same string shown here). |
| Each ClassLibrary is in a SubFolder | `LibInSubFolder` | Checked (`TRUE`) | Only used when *Keep the original VFP Project Folder Structure* is unchecked. Within the Libs folder, each VCX gets its own subfolder named after the library file, instead of all library classes landing in one shared Libs folder. |
| Empty the destination Folder (if already exist) | `EmptyFolder` | Checked (`TRUE`) | Clears the output folder before exporting, so stale files from a previous export don't linger alongside the new ones. |
| Add each Library in is own project | `SeparateLibraryProjects` | Checked (`TRUE`) | Each VCX dependency is exported as its own `.xsproj` (ClassLibrary), with proper `ProjectReference` entries wired up for cross-library dependencies. Unchecked: all library classes are combined into a single monolithic `ClassLibraries.xsproj`. Works best combined with *Each ClassLibrary is in a SubFolder*. |
| Place solution and project in the same directory | `PlaceSolutionInSameDirectory` | Unchecked (`FALSE`) | See **"Export folder layout"** in `README.md`. Checked: the main project's own files sit directly alongside the `.sln`. Unchecked: they go one level deeper, in a subfolder named after the project/solution. |
| Add each ClassLibrary namespace in the common Header file | `AddLibraryNamespace` | Checked (`TRUE`) | Controls whether the `USING <namespace>` lines for each library, appended to the shared `VFPXPorter.xh` header, are emitted as live statements or left commented out. |
| **Project file format** group | | | Format of the generated `.xsproj` and solution files. See **"Choosing the project format"** in `ExportAdvices.md`. Controls that don't apply to the selected format are disabled. |
| Project format | `ProjectFormat` | `Classic` | `Classic`: MSBuild 2003 project (`ToolsVersion="4.0"`, explicit framework references, `TargetFrameworkVersion`). `SDK-style`: `<Project Sdk="Microsoft.NET.Sdk">` (`VSSDKProject.prg`). Can be overridden for one export in the Export Project window. |
| Target framework | `TargetFramework` | `net472` | SDK-style only. Editable list (`net472`, `net48`, `net481`, `net8.0-windows`, `net9.0-windows`); any other value is written as-is, without validation. Use the `-windows` form for .NET 5+ (WinForms). Can be overridden for one export. |
| Framework version | `FrameworkVersion` | `4.7.2` | Classic only. Written as `TargetFrameworkVersion`. |
| X# references | `XSharpReferences` | `NuGet package` | SDK-style only. *NuGet package*: one `PackageReference` to `XSharp.VFP` (it contains `XSharp.VFP.UI` and brings `XSharp.RT` / `XSharp.Core` as dependencies). *Installed assemblies*: `Reference` + `HintPath` to `<X# folder>\Assemblies`; only possible for .NET Framework targets — for any other target the package is used and a Warning is logged. |
| X# package version | `XSharpPackageVersion` | *(empty)* | SDK-style with the package only. Empty: the version of the installed X# (highest `XSharp.VFP.<version>.nupkg` in `<X# folder>\Nuget`, X# folder read from the registry), shown under the field; `*` if X# is not found (a Warning is logged: a floating version can pick an older package from nuget.org). Any other value is written as-is. |
| Solution format | `SolutionFormat` | `.sln` | `.sln` or `.slnx` (XML solution, Visual Studio 2022 17.13 and later). Independent of the project format. *Append to existing Solution* looks for a solution file with the selected extension. |

---

## Export Project window

Per-export choices: they are not saved, and start from the Settings values each time the window
opens.

| Control | Setting | Default | Effect on export |
|---|---|---|---|
| Project Type | `OutputType` | Windows EXE | `OutputType` of the main project: Windows EXE (`WinExe`), Class Library (`Library`) or Console App (`Exe`). Library projects are always `Library`. |
| Append to existing Solution | `AppendToSolution` | Unchecked | Adds the exported projects to an existing solution named after *Solution Name* (with the extension of the *Solution format* setting) instead of creating a new one. Projects already in the solution are kept, including, for `.slnx`, their type and solution folder. |
| Solution Name | `SolutionName` | the `.pjx` name | Name of the solution file, and of the project subfolder (see *Place solution and project in the same directory*). |
| Place solution and project in the same directory | `PlaceSolutionInSameDirectory` | from Settings | Same as the Project tab setting, for this export. |
| Also save in XIDE Format | `SaveAsXIDE` | Unchecked | Also writes an XIDE project (`.xiproj`) next to the solution, from the same project list. |
| Project format | `ProjectFormat` | from Settings | Overrides the Settings value for this export. |
| Target framework | `TargetFramework` | from Settings | Overrides the Settings value for this export (SDK-style only). |
| *Solution: .sln / .slnx* | `SolutionFormat` | from Settings | Read-only reminder of the solution format that will be written. |

---

## Notes on defaults and precedence

- Defaults for every setting live in two places that must be kept in sync conceptually:
  `ExporterSettings.prg` (what `ReadValue(...)` falls back to when nothing is stored yet) and
  `XPorterSettings` constructor (what a brand-new `XPorterSettings{}` gets before
  `ExporterSettings:ToXPorterSettings()` overwrites it). The dialog only ever edits the
  `ExporterSettings`/`.ini` layer; `XPorterSettings` is rebuilt fresh from it on every export run.
- `ClassNamePrefix` and `DataFolder` are the two settings held as **static** properties on
  `XPorterSettings` rather than per-instance, because some low-level classes (`BaseItem`,
  `SCXVCXFile`) need to read them without holding a reference to the settings object.
- Several checkboxes only take effect through interaction with the JSON rule files in the
  ressources folder (`EventRules.json`, `Statements.json`) — toggling the checkbox changes
  *how* those rules are applied, not the rules themselves.
- `AliasTypeCollisions.json` is a flat list of cursor/alias names known to collide with a
  resolvable .NET/X# type (e.g. `Currency`, which collides with a Currency type), so an
  expression like `Currency.theField` would otherwise bind to that type's member instead of
  the DBF field. For every name in the list, `CodeConverter.ChangeAliasTypeCollisions()`
  rewrites `Name.` to `Name->` (unambiguous alias-field syntax in X#). **Currently suspended**:
  the calls are commented out in `CodeConverter` ("side effects on generated code", 2026-07-27),
  so the file is loaded but has no effect on the output.
- The six *Project file format* settings can also be set from the command line
  (`-projectFormat`, `-targetFramework`, `-frameworkVersion`, `-xsharpReferences`,
  `-xsharpVersion`, `-solutionFormat`); see `Source\VFPXPorterCmd\README.md`.
