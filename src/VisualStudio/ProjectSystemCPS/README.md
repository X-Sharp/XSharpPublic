# X# project system for SDK-style projects (CPS)

`XSharp.ProjectSystemCPS` loads **SDK-style X# projects** (`<Project Sdk="…">`) in Visual Studio with the
**Common Project System (CPS)**, the project system that Visual Studio also uses for SDK-style C#, VB and F#
projects. Legacy (non-SDK) X# projects keep using the classic X# project system, a fork of MPFproj
(`ProjectBase` and `ProjectPackage`). Both run side by side; a project selector decides per project.

The X# language service and code model (`XSharpCodeModelXs`) are unchanged. This assembly only replaces the part
that feeds them: instead of walking the MPFproj hierarchy and reading a response file, it takes the project items,
references and compiler options from CPS and its design-time builds.

Contents:

1. [What you get](#1-what-you-get)
2. [Supported environments](#2-supported-environments)
3. [Installation](#3-installation)
4. [How it works](#4-how-it-works)
5. [Source layout](#5-source-layout)
6. [Building](#6-building)
7. [Testing and debugging](#7-testing-and-debugging)
8. [Troubleshooting](#8-troubleshooting)
9. [Known limitations and open issues](#9-known-limitations-and-open-issues)
10. [References](#10-references)

---

## 1. What you get

For SDK-style X# projects, Visual Studio offers what it offers for SDK-style C# projects, plus the X# specific
parts:

- **Solution Explorer:** the Dependencies node (packages, assemblies, projects, COM, frameworks), file globbing,
  "Edit Project File", nesting of dependent files (`.designer.prg`, `.rc`, VO binaries), X# icons for the project,
  source files, forms, user controls and VO binaries.
- **NuGet:** `PackageReference`, restore, "Manage NuGet Packages".
- **Properties:** the new project properties editor with the managed pages (Application, Build, Build Events,
  Package, Debug, …) and the X# pages Language and Dialect.
- **Debugging:** launch profiles (`Properties/launchSettings.json`) and the framework dropdown of multi-target
  projects.
- **Publish and Pack** (`.nupkg`), as for C# projects.
- **IntelliSense without a prior build:** completion, navigation and the other features of the X# language service
  work as soon as the design-time build has run. A real build is not needed.
- **Error squiggles** for build errors, and **Task List** entries for comment tokens (`// TODO` …).
- **Designers:** the VO designers (window, menu, DBServer, FieldSpec) and the WinForms designer through the
  *shadow designer*; "Add .designer file" for forms written in a single file.

---

## 2. Supported environments

| Environment | SDK-style X# projects | Legacy X# projects |
|---|---|---|
| Visual Studio 2022 (17.x) | CPS (this assembly) | MPFproj |
| Visual Studio 2026 (18.x) | CPS (tested with 18.10) | MPFproj |
| Visual Studio 2019 (16.x) | not supported ("cannot be loaded inside this version of Visual Studio", as before) | MPFproj |

`XSharp.ProjectSystemCPS` only uses the public CPS SDK (`Microsoft.VisualStudio.ProjectSystem.Sdk` 17.9). It does
not reference the private assemblies of the managed project system (`Microsoft.VisualStudio.ProjectSystem.Managed*.dll`),
because they have no package and change their version with every Visual Studio release.

---

## 3. Installation

### 3.1 What has to be installed

The CPS project system needs two parts, which **must come from the same build**:

1. **The VSIX** (`XSharp.Project2022.vsix`, built by `ProjectPackage2022`). It contains `XSharp.ProjectSystemCPS.dll`,
   its pkgdef (package, project type, project selector) and `XSharp.ProjectSystemCPS.imagemanifest` (icons).
2. **The X# MSBuild support files** in the X# `MsBuild` folder (`XSharpBuildTask`, installed by the X# setup):

   | File | Purpose |
   |---|---|
   | `XSharp.CurrentVersion.targets`, `XSharp.CrossTargeting.targets` | import `XSharp.DesignTime.targets` when the project is loaded by CPS |
   | `XSharp.DesignTime.targets` | the CPS design-time integration (managed design-time targets, capabilities, rules) |
   | `XSharp.SDK.Props` | item globs of SDK-style projects; excludes the shadow designer companion folders |
   | `Rules\ProjectItemsSchema.XSharp.xaml` | file extensions → item types (Compile, None, NativeResource, VOBinary) |
   | `Rules\XSharpCompilerCommandLineArgs.xaml`, `Rules\XSharpProjectProperties.xaml` | design-time data for the code model |
   | `Rules\NativeResource*.xaml`, `Rules\VOBinary*.xaml` | properties of `.rc` files and VO binaries |
   | `Rules\ApplicationPropertyPage.XSharp.xaml`, `BuildPropertyPage.XSharp.xaml`, `LanguagePage.XSharp.xaml`, `DialectPage.XSharp.xaml`, `ReferencesPage.XSharp.xaml` | the property pages |

   `XSharp.Build.csproj` copies the rules to its output folder (`CopyToOutputDirectory`); **the X# installer must ship
   the `Rules` folder** together with the targets.

### 3.2 Finding the MSBuild support files

The project selector checks the support files before it sends a project to CPS
(`Selection/XSharpMsBuildSupport.cs`). It looks for the X# `MsBuild` folder like `XSharp.BeforeCommon.Props` does:

1. the environment variable `XSharpMsBuildDir`,
2. the registry value `XSharpPath` (`HKLM\SOFTWARE\WOW6432Node\XSharpBV\XSharp` or `HKLM\SOFTWARE\XSharpBV\XSharp`)
   plus `MsBuild`,
3. `%ProgramFiles(x86)%\XSharp\MsBuild`.

CPS is used when `XSharp.CurrentVersion.targets` there contains the `XSharpCpsProjectSystem` import and
`Rules\XSharpCompilerCommandLineArgs.xaml` exists.

### 3.3 Mismatched installations

- **VSIX newer than the X# installation** (support files without the CPS import): SDK-style projects are not loaded.
  The selector sends them to MPFproj, which shows "The project file … is an SDK style project. SDK style X# projects
  are loaded by the X# project system for SDK style projects, which needs the X# MSBuild support files of a matching
  X# installation. Please run the X# setup program again." Legacy projects are not affected.
- **X# installation newer than the VSIX:** the targets only import the design-time integration when the global
  property `XSharpCpsProjectSystem` is `true`, which only this project system sets. Command-line builds and MPFproj
  projects are unchanged.

### 3.4 Assemblies in the GAC

The X# setup installs `XSharp.CodeDomProvider`, `XSharp.VSParser` and `XSharp.Evaluator` in the GAC. Visual Studio
always loads the GAC copy when its identity matches, also for development builds of the VSIX. A change to one of these
assemblies (for example the CodeDOM generator used by "Add .designer file" and the designers) only becomes effective
in Visual Studio with a matching X# installation.

---

## 4. How it works

### 4.1 Project selection

Solutions reference X# projects with the MPFproj project type GUID `{AA6C8D78-22FF-423A-9C7C-5F2393824E04}`. The CPS
project type has its own GUID `{AB494DCE-A11B-4583-851E-D4A86775B3F0}`.

`Selection/XSharpProjectSelector` is an `IVsProjectSelector` (the mechanism the managed project system uses for
`FSharpProjectSelector`), registered for **both** project types (`Selection/ProvideProjectSelectorAttribute`,
`Projects\{type}\ProjectSelector`). For every X# project that is opened it decides:

- **CPS** when the project file is SDK-style (`<Project Sdk>`, `<Import Sdk>` or `<Sdk Name>`,
  `Selection/SdkProjectFile`) **and** the MSBuild support files support CPS (3.2);
- **MPFproj** otherwise, also when the file cannot be read.

The package `XSharpProjectSystemPackage` registers the selector object with `IVsRegisterProjectSelector` when it
initializes; Visual Studio loads the package on demand.

When Visual Studio saves a solution it writes the GUID of the project system that actually loaded the project, so the
entry of an SDK-style project changes once from `{AA6C8D78…}` to `{AB494DCE…}` (C# SDK projects do the same:
`{FAE04EC0…}` → `{9A19103F…}`; Visual Studio has no way to prevent it). Because the selector is registered for the CPS
GUID as well, such an entry still goes to MPFproj when the project is legacy or the support files are too old.

The X# project templates still use the MPFproj project type (template language `XSharp`); SDK-style templates reach
CPS through the selector. The CPS project type has its own template language `XSharpCps`, so no template is offered
twice.

### 4.2 Project type and capabilities

`XSharpProjectSystemPackage` registers the CPS project type (`[ProjectTypeRegistration]`) with the initial
capabilities of `XSharpCapabilities.ProjectTypeCapabilities`:

```
XSharpCps; AppDesigner; HandlesOwnReload; OpenProjectFile; PreserveFormatting;
ProjectConfigurationsDeclaredDimensions; ProjectPropertiesEditor; .NET; UseProjectEvaluationCache
```

This is the set of the managed C# project type without `LanguageService` (X# has its own language service, not the
Roslyn workspace), `CSharp`, `SharedImports` and `EditAndContinue`. `XSharpCps` is the gate for all MEF parts of this
assembly (`[AppliesTo("XSharpCps")]`).

`XSharp.DesignTime.targets` adds the MSBuild capabilities of `Microsoft.Managed.DesignTime.targets`
(`DependenciesTree`, `PackageReferences`, `AssemblyReferences`, `ProjectReferences`, `COMReferences`, `LaunchProfiles`,
`Publish`, `UseFileGlobs`, …) plus `XSharp; Managed; SingleTargetBuildForStartupProjects; ProjectPropertiesEditor`, and
removes `LanguageService` and `ReferencesFolder`.

### 4.3 MSBuild integration

- `Evaluation/XSharpCpsGlobalPropertiesProvider` sets the **global property `XSharpCpsProjectSystem=true`** for projects
  of the CPS project type.
- `XSharp.CurrentVersion.targets` and `XSharp.CrossTargeting.targets` import `XSharp.DesignTime.targets` only when that
  property is `true` and the files exist (`$(XSharpDesignTimeTargetsPath)`, `$(XSharpManagedDesignTimeTargetsPath)`,
  both overridable). MPFproj projects and command-line builds are therefore not affected.
- `XSharp.DesignTime.targets` imports the managed design-time targets from the Visual Studio installation (like F#),
  sets the X# capabilities and rules, and points `AddItemTemplatesGuid`, `GeneratorsTypeGuid` and `CmdUIContextGuid`
  to the MPFproj project factory, where the X# package already registers the item templates and single-file
  generators. **Add New Item** therefore offers the X# templates.
- `XSharpCpsProjectSystem` is appended to `_GlobalPropertiesToRemoveFromProjectReferences`, so referenced projects
  (C#, legacy X#) are built with their own global properties and are not built twice.
- `Rules/ProjectItemsSchema.XSharp.xaml` maps the file extensions: `.prg/.prgx/.xs` → `Compile`, `.vh/.xh/.ch` → `None`,
  `.rc` → `NativeResource`, `.xsfrm/.vnfrm/.xsdbs/.vndbs/.xsmnu/.vnmnu/.xsfs/.vnfs/.xssql/.vnsqs/.xsrep/.vnrep` → `VOBinary`.
  The globs themselves come from the .NET SDK and `XSharp.SDK.Props` (with `DependentUpon` for designer files, `.rc`
  files and VO binaries).

### 4.4 Feeding the code model

`LanguageService/XSharpProjectAdapter` is the core. It is an `IProjectDynamicLoadComponent` (one per project,
`UnconfiguredProject` scope) and implements `IXSharpProject`, the interface through which the code model talks to a
project system. When a project loads it:

1. makes sure the X# solution model (`XSolution`) is open. If the X# package has not opened it yet (a pure CPS
   solution, a project opened without a `.sln`), the adapter loads the X# package and opens the model for the solution
   file, also when the solution has not been saved yet (`.vs\{name}\X#Model.xsdb`);
2. creates the `XProject` **on the UI thread**, like MPFproj (parallel creation raced on the X# database);
3. subscribes to the **active configuration** through `IActiveConfiguredProjectSubscriptionService`:

   | Subscription | Data | Code model |
   |---|---|---|
   | `SourceItemsRuleSource` (rule names from `SourceItemRuleNamesSource`) | Compile, None, Content, EmbeddedResource, Page, ApplicationDefinition, Resource, VOBinary, NativeResource items | `AddFile` / `RemoveFile` (a rename is remove + add) |
   | `ProjectRuleSource` (`XSharpProjectProperties`, `EvaluatedProjectReference`) | RootNamespace, IntermediateOutputPath, TargetPath, NS, project references | properties; provisional parse options (dialect only) until the first design-time build; `AddProjectReference` / `RemoveProjectReference` |
   | `ProjectBuildRuleSource` (`XSharpCompilerCommandLineArgs`) | the compiler command line of the design-time build (`CompileDesignTime`) | `XParseOptions.FromVsValues`, `RefreshReferences`, `ResolveReferences` |

   `XSharpCommandLine` converts the Xsc switches into the format of `FromVsValues` (the same result as the old
   response-file path). A failed design-time build (empty result) keeps the previous state.
4. starts the model walker (`ModelWalker.AddProject`) after file changes and design-time builds.

Two locks keep this consistent: `modelGate` serializes all changes of the code model, `gate` only protects the file set
(so `HasFileNode`, called by the designers on the UI thread, never waits for database I/O).

`LanguageService/SourceFileWatcher` (a `FileSystemWatcher` on the project folder) walks source files of the project
that change on disk 500 ms after the last change: saves in the editor, files written by the designers, external tools,
source control. After a buffer overflow the whole project is walked again. Linked files outside the project folder are
not watched.

`AddFileNode` / `DeleteFileNode` (called by the VO designers after they wrote or before they delete a generated file)
register the file with the code model immediately; the project file does not change, because the SDK globs include the
files.

### 4.5 Errors and the Task List

- `LanguageService/BuildErrorLoggerProvider` (`IBuildLoggerProviderAsync`) attaches an MSBuild logger to real builds
  (Build, Rebuild, Clean; not design-time builds) and passes the errors and warnings to the adapter
  (`SetBuildErrors`, per configuration). `IntellisenseErrorStore` merges them with the code model errors; the X#
  error tagger draws the **squiggles** from them. Build errors are not added to the Error List again, Visual Studio
  already shows them.
- `LanguageService/IntellisenseErrorList` shows code model errors in the Error List. The code model currently reports
  none (as with MPFproj), so the list stays empty.
- `LanguageService/CommentTaskList` shows the comment tasks of the project in the Task List after each walk.
  `CommentTokens` passes the Task List tokens (Tools > Options > Environment > Task List) to the code model when no
  MPFproj project did.

### 4.6 Property pages

The project properties are the new properties editor of Visual Studio (capability `ProjectPropertiesEditor`) with XAML
rules. `PropertyPages/XSharpProjectDesignerPageProvider` is needed so that Visual Studio offers the project designer at
all (it only has providers for C#/VB/F#).

| Page | Rule | Content |
|---|---|---|
| Application | `ApplicationPropertyPage.XSharp.xaml` (extends the managed page) | manifest, `VulcanCompatibleResources`, `UseNativeVersion`, `NoWin32Manifest`; the C# Win32 resource choice is hidden (X# compiles the `.rc` files instead) |
| Build | `BuildPropertyPage.XSharp.xaml` (extends the managed page) | other flags (`CommandLineOption`), shared compiler, warning level, `SuppressRCWarnings`, conditional compilation symbols; C#-only options hidden |
| Language | `LanguagePage.XSharp.xaml` | dialect, named arguments, unsafe, case sensitivity, overflow checks, include paths, standard defines, … |
| Dialect | `DialectPage.XSharp.xaml` | VO1–VO17, LB, XPP1, FOX2, FOX3 (depending on the dialect) |
| Global Usings | `ReferencesPage.XSharp.xaml` | `ImplicitUsings` |
| Build Events, Debug, Package, … | managed pages | |

**Conditional compilation symbols:** the managed Build page edits `DefineConstants` with a list editor whose value is a
list of name/value pairs. For C# a value provider of the managed project system converts it; for X# nothing did, so the
editor wrote `MYSYM=False` into the project file and lost the implicit `DEBUG;TRACE`.
`BuildPropertyPage.XSharp.xaml` therefore defines `DefineConstants` with its own persistence `XSharpDefineConstants`,
implemented by `PropertyPages/XSharpDefineConstantsPropertiesProvider`: it wraps the project file provider, shows only
the project's own symbols (after `$(DefineConstants);`), writes `$(DefineConstants);MYSYM` and deletes the property
when the list is empty. `PropertyPages/KeyValuePairListEncoding` is the list format of the editors.

`<Using>` items (global usings with alias or static) are edited in the project file; the C# "Manage Implicit Global
Usings" editor needs a value provider that only exists for C#.

Changing a property triggers a design-time build; the adapter picks up the new parse options from it.

### 4.7 Solution Explorer icons

`Imaging/XSharpProjectTreePropertiesProvider` sets the X# icons (`XSharp.ProjectSystemCPS.imagemanifest`): the project,
`.prg/.prgx/.xs` files, VO binaries, and forms and user controls. A `.prg` is a form or user control when its `SubType`
says so or, since SDK-style projects rarely have one, when a matching `.Designer.prg` exists and its class inherits
from `Form`/`UserControl` (also over a line continuation, the format of the CodeDOM generator).

CPS only recalculates the icons of nodes that changed in a tree update. Adding a `.Designer.prg` below an existing
`.prg` does not change the `.prg` node, so `Imaging/XSharpProjectTreePropertiesProviderSource`
(`IProjectTreePropertiesProviderDataSource`) supplies the provider and publishes a new instance when a `.Designer.prg`
is added or removed; CPS then recalculates all nodes. Its values carry their own version key: with the versions of the
source items subscription the tree waited for ever.

The image library resolves the manifest's pack URIs at startup, so the package has `[ProvideBindingPath]` and
`ProjectPackage2022` a `ProvideCodeBase` for `XSharp.ProjectSystemCPS`.

### 4.8 VO designers

The VO window, menu, DBServer and FieldSpec editors are registered by file extension in the X# package and do not
depend on the project system. They get the project through `XSolution.FindFile(file).Project.ProjectNode`, which is the
adapter for files of CPS projects. They write the generated `.prg`/`.rc` files to disk and call
`HasFileNode`/`AddFileNode`/`DeleteFileNode`; `SourceFileWatcher` makes the code model read the regenerated files.

### 4.9 WinForms designer (shadow designer)

The Visual Studio WinForms designer only works with C# and VB projects that the managed project system loaded (for
.NET it runs out of process and needs the project's references, target framework and Roslyn workspace). The shadow
designer therefore opens X# forms through a generated **companion C# project**:

1. **Companion:** `ShadowDesignerBridge` converts the form (`Form1.prg` plus `Form1.Designer.prg`) with the X# CodeDOM
   parser and the C# code provider into `Form1.Designer.cs` and a stub `Form1.cs` (same base class and imports), in a
   companion project `<Project>.ShadowDesigner\<Project>.ShadowDesigner.csproj` next to the project folder
   (`CompanionProjectWriter`). The companion gets the target framework of the loaded project and its resolved
   references. `XSharp.SDK.Props` excludes `**/*.ShadowDesigner/**` from X# projects.
2. **Solution:** `SolutionWiring` adds the companion to the solution folder "Shadow Designer (generated)" and opens
   `Form1.Designer.cs` in the designer.
3. **Saving:** `CompanionSaveWatcher` and `DesignerChangesSync` write the designer's changes back as X#
   (`Form1.Designer.prg`) with the X# CodeDOM generator; `EventHandlerSync` adds event handlers to `Form1.prg`.
   The context menu commands "Sync Designer Changes From Shadow Designer" and "Sync Event Handlers From Shadow Designer"
   do the same on demand.
4. **Cleanup:** `ShadowDesignerCleanup` removes the companion projects and their folders when the solution closes
   (with retries while the designer host still locks a folder, and cancellation when the companion is used again).

Entry points in CPS projects: double click / Enter (`XSharpShadowDesignerDefaultActionCommand`) and View Designer
(`XSharpShadowDesignerViewFormCommand`) on a `.prg` that has a `.Designer.prg`. Forms created with **Add New Item**
open in the designer too: the template wizard opens them in the code editor, so `NewFormDesignerRedirect` switches a
form created within the last 30 s to the designer.

**References:** the designer needs the assemblies of the project's packages. When they are not known yet (the solution
has just been opened, the NuGet restore is still running), `ShadowDesignerBridge.EnsureReferencesAsync` waits for the
IntelliSense stage of the solution load and then up to 20 s for the design-time build, with a status bar message. If
they are still missing it offers a build ("Build now?"; Tools > Options > X# Custom Editors > Other Editors,
"Auto-build for Shadow Designer", builds without asking) and waits up to 3 s after it. CPS runs command handlers while
it waits synchronously on the UI thread, so in that case the command returns at once and the designer opens in the
background; if it cannot be opened, the form opens in the code editor.

### 4.10 Commands of the X# package

Most commands of `ProjectPackage/Commands` are global (XPorters, UDC tester, repository window, …) and work unchanged.
Project-specific ones:

- **Add .designer file** (context menu of a form `.prg`): splits a form written in a single file into `Form.prg` and
  `Form.designer.prg`. For CPS projects the form is recognized through the code model (base classes of the first
  class), the CodeDOM provider works on the path, and the new file needs no project item (globs). The form is parsed
  first; when the project is still loading and the form class cannot be recognized, a message asks to try again.
- **Convert to Windows Form**, **Sync …** commands: work for CPS projects.
- **Edit project file**: only for MPFproj projects; CPS has its own "Edit Project File".

### 4.11 Multi-targeting

Projects with `<TargetFrameworks>` build and debug all target frameworks (framework dropdown, launch profiles). The
code model follows the **active configuration**, which CPS sets to the first entry of `<TargetFrameworks>`; the
framework dropdown only selects what F5 starts. IntelliSense therefore uses the references and defines of the first
target framework. This matches the default editor context of C#, which in addition offers a context dropdown in the
editor (one Roslyn project per target framework); the X# code model has one context per project.

---

## 5. Source layout

```
src/VisualStudio/ProjectSystemCPS/
  XSharpProjectSystemPackage.cs      package, CPS project type, project selector registration
  XSharpCapabilities.cs              capability names and the initial capabilities
  Evaluation/
    XSharpCpsGlobalPropertiesProvider.cs   global property XSharpCpsProjectSystem=true
  Selection/
    XSharpProjectSelector.cs          IVsProjectSelector: SDK-style → CPS, else MPFproj
    ProvideProjectSelectorAttribute.cs     pkgdef registration of the selector
    SdkProjectFile.cs                 "is this an SDK-style project file"
    XSharpMsBuildSupport.cs           do the installed MSBuild support files support CPS
  LanguageService/
    XSharpProjectAdapter.cs           IXSharpProject: feeds XProject from CPS
    XSharpCommandLine.cs              Xsc switches → XParseOptions.FromVsValues
    SourceFileWatcher.cs              walks source files changed on disk
    BuildErrorLoggerProvider.cs       build errors for the squiggles
    IntellisenseErrorStore.cs         code model and build errors per file
    IntellisenseErrorList.cs          Error List provider
    CommentTaskList.cs, CommentTokens.cs   Task List
  PropertyPages/
    XSharpProjectDesignerPageProvider.cs   enables the project designer
    XSharpDefineConstantsPropertiesProvider.cs   persistence "XSharpDefineConstants"
    KeyValuePairListEncoding.cs       list format of the property editors
  Imaging/
    XSharpImages.cs                   image monikers
    XSharpProjectTreePropertiesProvider.cs   icons
    XSharpProjectTreePropertiesProviderSource.cs   recalculates the icons when designer files change
  ShadowDesigner/                     WinForms designer through a companion C# project (4.9)
  XSharp.ProjectSystemCPS.imagemanifest, Resources/*.png

src/Compiler/src/Compiler/XSharpBuildTask/
  XSharp.DesignTime.targets           CPS design-time integration
  XSharp.CurrentVersion.targets, XSharp.CrossTargeting.targets   gated import
  XSharp.SDK.Props                    SDK item globs
  Rules/*.xaml                        items schema, design-time data, property pages

src/VisualStudio/ProjectSystemCPS.TestBed/   test projects and StartExp.cmd (7.1)
```

Other parts involved:

- `src/VisualStudio/XSharpCodeModelXs` — the code model (`XProject`, `XSolution`, `IXSharpProject` in `Interfaces.prg`),
  unchanged.
- `src/VisualStudio/ProjectPackage` — the X# package (language service registration, editors, commands, MPFproj
  project type). `ProjectPackage2022` references `ProjectSystemCPS` and ships it in its VSIX.
  `XSharpProjectFactory.CreateProject` rejects SDK-style projects that reach MPFproj.
- `src/VisualStudio/ProjectBase` — the MPFproj fork, for legacy projects only.
- `src/VisualStudio/CodeDomProvider` — the X# CodeDOM provider (designers, "Add .designer file").

Rules for changes:

- `ProjectSystemCPS` references `XSharpCodeModelXs` (and `CodeDomProvider`), **never** `ProjectBase` or `ProjectPackage`;
  the dependency goes the other way.
- Only the public CPS SDK; no reference to `Microsoft.VisualStudio.ProjectSystem.Managed*.dll`.
- Keep the public API of `XSharpCodeModelXs` unchanged.

---

## 6. Building

`ProjectSystemCPS` is part of `src/VSIntegration2022.sln` and is built with it (`src/buildvs2022.cmd Debug|Release`).
It is an SDK-style `net48` project; `ProjectPackage2022` puts the assembly, its pkgdef and the image manifest into its
VSIX.

To build the VSIX alone (from `src/VisualStudio`, with the Visual Studio 2022 MSBuild):

```
msbuild ProjectPackage/ProjectPackage2022.csproj -p:Configuration=Debug -p:Platform=AnyCPU
```

- Pass `Platform=AnyCPU` (without a space); other values leave the `DEV17` define unset and give misleading errors.
- A Debug build **deploys** the VSIX into the experimental instance of Visual Studio 2022; the instance must be
  closed. `-p:DeployTargetInstanceId=<instance id>` deploys into another Visual Studio installation instead, for
  example Visual Studio 2026 (the instance id from `vswhere -all`).
- The Debug build of `ProjectPackage2022` overwrites the Visual Studio 2019 manifest
  `ProjectPackage/source.extension.vsixmanifest`; restore it (`git checkout`) before committing.
- The MSBuild support files (3.1) are built by `XSharp.Build.csproj` (`src/buildcompiler.cmd`).

---

## 7. Testing and debugging

### 7.1 Test bed

`src/VisualStudio/ProjectSystemCPS.TestBed`:

| Project / solution | Purpose |
|---|---|
| `CpsTestBed.xsproj`, `CpsTestBed.sln` | SDK-style console project (Core dialect, net48); the solution references it with the CPS GUID |
| `Legacy/LegacyTestBed.xsproj` | legacy project, must stay on MPFproj |
| `WinForms/WinFormsTestBed.xsproj` | SDK-style WinForms project (net8.0-windows) for the shadow designer |
| `VO/VOTestBed.xsproj` | SDK-style copy of the VO SDI Application template (net48, x86) for the VO designers |
| `SelectorTestBed.sln` | all four projects with the MPFproj GUID: tests the selector |

`StartExp.cmd [solution]` starts the Visual Studio 2022 experimental instance for tests with the MSBuild support files
of the repository **without changing the installed X#**: it copies the installed X# `MsBuild` folder to
`Artifacts\CpsMsBuild`, copies the changed repository files over it (`XSharp.CurrentVersion.targets`,
`XSharp.CrossTargeting.targets`, `XSharp.DesignTime.targets`, `XSharp.SDK.Props`, `Rules`), deletes the image library
cache of the experimental instance and starts `devenv /rootSuffix Exp` with `XSharpMsBuildDir` pointing to the copy.
Default solution: `CpsTestBed.sln`. For Visual Studio 2026, start its `devenv.exe /rootSuffix Exp <solution>` the same
way with `XSharpMsBuildDir` set to `Artifacts\CpsMsBuild`.

After a test, reset the test bed: Visual Studio rewrites the project type GUIDs in `SelectorTestBed.sln` (4.1), and the
designers rewrite the files of `WinForms` and `VO`.

For a "never built" state, delete `bin`, `Artifacts\obj\<Project>` and the `.vs` folder of the test bed (the X#
database `X#Model.xsdb`).

### 7.2 Logs

- **X# log:** `%TEMP%\XSharp.Intellisense\Project_*.log`, one file per Visual Studio session. Useful entries:
  `XSharpProjectSelector: <file> -> CPS|MPFproj`, `XSharpMsBuildSupport: …`,
  `XSharpProjectAdapter: … design-time build, N options, M references, dialect …`, `… files added, … files removed`,
  `… changed on disk, walking it again`, `ShadowDesignerBridge: …`, `ShadowDesignerCleanup: …`.
- **ActivityLog:** `%APPDATA%\Microsoft\VisualStudio\<instance>\ActivityLog.xml` (UTF-16). Package load failures
  (`SetSite failed for package [XSharpProjectSystemPackage]`) are logged even without `/log`. Pass the solution before
  `/log` (`devenv <solution> /log`), otherwise Visual Studio writes the log into the solution file.
- **MEF errors:** `%LOCALAPPDATA%\Microsoft\VisualStudio\<instance>\ComponentModelCache\Microsoft.VisualStudio.Default.err`.
- **Binlogs of Visual Studio builds:** start Visual Studio with `MSBUILDDEBUGENGINE=1` and `MSBUILDDEBUGPATH=<folder>`;
  every build and design-time build writes a binlog there.

### 7.3 Useful techniques

- **Hangs:** attach non-invasively and dump the managed stacks:
  `cdb -pv -p <pid> -c ".loadby sos clr; !EEStack -EE; qd"`. A UI thread in CPS `ProjectNode.HrInvoke` with no thread
  working on the project tree means that the tree waits for a data source.
- **Assembly resolution:** Visual Studio resolves extension and platform assemblies through the per-instance
  `%LOCALAPPDATA%\Microsoft\VisualStudio\<instance>\devenv.exe.config`, generated from the pkgdef `RuntimeConfiguration`
  entries. The merged configuration is in `privateregistry.bin` of the instance; it can be read without admin rights
  with `RegLoadAppKey` while Visual Studio is closed.
- **NuGet scenarios:** a `PackageReference` with `RestoreSources` (a local folder feed) and `RestorePackagesPath` in the
  project makes the restore controllable (delete the packages folder and `project.assets.json`: restore at load; rename
  the feed: the restore fails). Keep the paths short: NuGet inside Visual Studio does not extract files beyond 260
  characters.
- **GAC'd assemblies** (3.4) can be tested outside Visual Studio with a small console program that has
  `<developmentMode developerInstallation="true"/>` in its `.config` and `DEVPATH` pointing to the build output.

---

## 8. Troubleshooting

| Symptom | Cause and remedy |
|---|---|
| "The project file … is an SDK style project … Please run the X# setup program again." | The selector sent the project to MPFproj. Either the MSBuild support files are too old (install the matching X#; the X# log shows `XSharpMsBuildSupport: … does not support CPS`), or the CPS package did not load (ActivityLog: `SetSite failed for package [XSharpProjectSystemPackage]`). |
| `SetSite failed … Could not load file or assembly 'Microsoft.VisualStudio.ProjectSystem.VS, Version=17.0.0.0'` in an experimental instance | The configuration of that instance lacks the CPS `codeBase` entries (seen with an instance created before a Visual Studio update). Reset it: `CreateExpInstance.exe /Reset /VSInstance=<version>_<id> /RootSuffix=Exp` (`VSSDK\VisualStudioIntegration\Tools\Bin`), then deploy again. This deletes the settings and extensions of that experimental instance. |
| No X# icons | The image library cache does not contain the X# manifest. Delete `ImageLibrary\ImageLibrary.cache` of the instance (`StartExp.cmd` does this) and restart. |
| No IntelliSense, completion empty | Check the X# log for the design-time build line of the project. If it is missing, check the Output window (Build) and the binlog of the design-time build; if the adapter reports 0 references, the design-time build failed. |
| "Could not find the form class …" from "Add .designer file" | The project is still loading and the references are not known yet; try again when completion works in the file. |
| The shadow designer shows "… is not contained within a project that supports code" | The companion project did not pick up the file; close the designer and open it again. Check the X# log (`ShadowDesignerBridge`). |
| The `.sln` changes the project type GUID of SDK-style projects | Expected, once (4.1). |
| "One or more projects in the solution were not loaded correctly" after Visual Studio was killed | The Output window (Solution) reports that the document restore is skipped once. Harmless. |

---

## 9. Known limitations and open issues

- **Multi-targeting:** IntelliSense uses the first target framework (4.11). A possible improvement is to follow the
  framework selected in the dropdown.
- **"Include files" folder:** the virtual folder of MPFproj that lists `#include` files does not exist for CPS projects.
- **Default items turned off:** in SDK-style projects with `EnableDefaultItems=false` (or
  `EnableDefaultCompileItems=false`), `.prg` files generated by the VO designers do not become part of the project,
  because the adapter relies on the globs. The X# globs of `XSharp.SDK.Props` (VO binaries, `.rc`, `*.designer.prg`,
  `*.resx`, `*.xaml.prg`) ignore these properties.
- **Shadow designer saves:** writing back rebuilds `Form.Designer.prg` from the fields and `InitializeComponent` only;
  other members (`Dispose`), regions and field initializers of that file are lost (the same logic existed before for
  the MPFproj SDK projects).
- **Shadow designer companion** is visible in Solution Explorer and written into the `.sln` while the solution is open.
- **Code model errors** are not reported by the code model, so the Error List shows only build errors (as with MPFproj).
- **Global usings editor:** `<Using>` items are edited in the project file (4.6).
- **Installation:** the VSIX and the MSBuild support files must match (3.3).
- **Visual Studio 2019** cannot load SDK-style X# projects.

---

## 10. References

- CPS: [microsoft/VSProjectSystem](https://github.com/microsoft/VSProjectSystem) (documentation of the Common Project
  System and its extensibility points).
- The managed project system (C#, VB, F#): [dotnet/project-system](https://github.com/dotnet/project-system), the
  reference for capabilities, rules, design-time builds, project selectors (`FSharpProjectSelector`) and the language
  service integration.
- The code model contract: `src/VisualStudio/XSharpCodeModelXs/Interfaces.prg` (`IXSharpProject`) and the smallest
  implementation, `OrphanedFilesProject.prg`.
