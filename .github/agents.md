# Agent Instructions for XSharpPublic

## Source Branch

Always take source from the **`dev`** branch. Do not use `master` or any other branch as a base unless explicitly instructed.

## Repository Layout

The repository root contains a top-level `src/` folder that is the home of all subsystems:

```
src/
├── Compiler/          # X# Compiler (ANTLR-based front-end, code analysis, code generation, xsc driver)
│   └── src/
│       └── Compiler/
│           ├── XSharpCodeAnalysis/    # Core compiler/code analysis
│           ├── XSharpCodeGenerator/   # Code generation
│           ├── xsc/                   # Compiler driver (xsc.exe)
│           ├── XSharpBuildTask/       # MSBuild task wrapping xsc
│           ├── XSFullMacroCompiler/   # Full macro compiler
│           └── XSVulcanMacroCompiler/ # Vulcan-compatible macro compiler
│
├── Roslyn/            # Modified Roslyn source (Microsoft.CodeAnalysis.*); changes are guarded by #if XSHARP
│
├── Runtime/           # X# Runtime libraries
│   ├── XSharp.Core/         # Core runtime (VOSDKTyped, low-level helpers)
│   ├── XSharp.RT/           # Main runtime (VO-compatible functions and classes)
│   ├── XSharp.Rdd/          # RDD subsystem (DBF, NTX, CDX, FPT, …)
│   ├── XSharp.SQLRdd/       # SQL RDD (ADO.NET-based RDD)
│   ├── XSharp.Data/         # Data helpers
│   ├── XSharp.VO/           # VO-dialect support
│   ├── XSharp.VFP/          # Visual FoxPro-dialect support
│   ├── XSharp.XPP/          # XBase++ dialect support
│   ├── XSharp.Harbour/      # Harbour dialect support
│   ├── MacroCompiler/       # Stand-alone macro compiler (used at runtime)
│   └── VOSDK/               # VO SDK compatibility layer
│
├── VisualStudio/      # VS Integration (project system, language service, debugger, designers)
│   ├── ProjectBase/         # Base project-system infrastructure
│   ├── ProjectPackage/      # X# project-system package (nodes, commands, templates)
│   ├── LanguageService/     # Classifier, completion, signature help, navigation bar, Roslyn pipeline
│   ├── XSharpCodeModelXs/   # Code model (file/project/solution parsing, symbol store)
│   ├── Debugger/            # Debugger integration
│   ├── Debugger.UI/         # Debugger UI components
│   ├── CodeDomProvider/     # CodeDOM provider
│   ├── CodeGenerator/       # Code generator (used by designers)
│   ├── AppDesigner/         # Application designer
│   └── XSharpVoEditors/     # VO-style form/menu/resource designers
│
└── Tools/             # Developer tools and utilities
    ├── VOXporter/           # VO → X# migration tool
    ├── VFPXPorter/          # VFP → X# migration tool
    ├── XPorter/             # Generic xporter helpers
    ├── Convert2SDKProject/  # Converts legacy projects to SDK-style
    ├── ExtractDocs/         # Documentation extractor
    ├── Mono.Cecil/          # Bundled Mono.Cecil (IL inspection)
    └── UDCTester/           # UDC (User-Defined Commands) tester
```

## Solution Files

| Solution | Purpose |
|---|---|
| `src/Master.slnx` | All subsystems combined |
| `src/Compiler.slnx` | Compiler only |
| `src/Runtime.slnx` | Runtime only |
| `src/VSIntegration2022.sln` | VS Integration (VS 2022) |
| `src/VSIntegration.sln` | VS Integration (VS 2019) |
| `src/Tools.slnx` | Tools only |
| `src/SqlRdd.slnx` | SQL RDD only |
| `src/MacroCompiler.sln` | Macro compiler only |

## Build Notes

- The VS Integration projects target **.NET Framework** and require a **Windows Visual Studio** environment; they cannot be built with plain `dotnet` on Linux.
- The compiler build depends on a customised Roslyn. Run `ContinuousIntegrationBuild.cmd` from the repository root to bootstrap everything.
- Individual subsystem build scripts are in `src/`: `buildcompiler.cmd`, `buildrt.cmd`, `buildvs2022.cmd`, etc.
- Build output lands in an `Artifacts/` folder at the repository root.
