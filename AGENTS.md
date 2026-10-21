# AGENTS.md — XSharpPublic Repository

## Overview

This repository contains the source code for the **XSharp** programming language ecosystem: the compiler, runtime libraries, Visual Studio integration, and developer tools. XSharp is a .NET-based xBase language with dialects compatible with Visual Objects (VO), Visual FoxPro (VFP), XBase++, and Harbour.

## Source Branch

Always take source from the **`dev`** branch. Do not use `master` or any other branch as a base unless explicitly instructed.

## Repository Layout

```
/
├── src/
│   ├── Compiler/          # X# Compiler (ANTLR-based front-end, code analysis, code generation)
│   │   └── src/Compiler/
│   │       ├── XSharpCodeAnalysis/        # Core compiler / code analysis
│   │       ├── XSharpCodeGenerator/       # Code generation
│   │       ├── xsc/                       # Compiler driver (xsc.exe)
│   │       ├── XSharpBuildTask/           # MSBuild task wrapping xsc
│   │       ├── XSFullMacroCompiler/       # Full macro compiler
│   │       └── XSVulcanMacroCompiler/     # Vulcan-compatible macro compiler
│   │
│   ├── Roslyn/            # Modified Microsoft Roslyn source; changes guarded by #if XSHARP
│   │
│   ├── Runtime/           # X# Runtime libraries
│   │   ├── XSharp.Core/          # Core runtime (low-level helpers)
│   │   ├── XSharp.RT/            # Main runtime (VO-compatible functions/classes)
│   │   ├── XSharp.Rdd/           # RDD subsystem (DBF, NTX, CDX, FPT, …)
│   │   ├── XSharp.SQLRdd/        # SQL RDD (ADO.NET-based)
│   │   ├── XSharp.Data/          # Data helpers
│   │   ├── XSharp.VO/            # Visual Objects dialect support
│   │   ├── XSharp.VFP/           # Visual FoxPro dialect support
│   │   ├── XSharp.XPP/           # XBase++ dialect support
│   │   ├── XSharp.Harbour/       # Harbour dialect support
│   │   ├── MacroCompiler/        # Stand-alone macro compiler (runtime use)
│   │   └── VOSDK/                # VO SDK compatibility layer
│   │
│   ├── VisualStudio/      # VS Integration (project system, language service, debugger, designers)
│   │   ├── ProjectBase/          # Base project-system infrastructure
│   │   ├── ProjectPackage/       # X# project-system package
│   │   ├── LanguageService/      # Classifier, completion, signature help, navigation
│   │   ├── XSharpCodeModelXs/    # Code model (file/project/solution parsing, symbol store)
│   │   ├── Debugger/             # Debugger integration
│   │   ├── Debugger.UI/          # Debugger UI components
│   │   ├── CodeDomProvider/      # CodeDOM provider
│   │   ├── CodeGenerator/        # Code generator (designers)
│   │   ├── AppDesigner/          # Application designer
│   │   └── XSharpVoEditors/      # VO-style form/menu/resource designers
│   │
│   ├── Tools/             # Developer tools and utilities
│   │   ├── VOXporter/            # VO → X# migration tool
│   │   ├── VFPXPorter/           # VFP → X# migration tool
│   │   ├── XPorter/              # Generic xporter helpers
│   │   ├── Convert2SDKProject/   # Converts legacy projects to SDK-style
│   │   ├── ExtractDocs/          # Documentation extractor
│   │   ├── Mono.Cecil/           # Bundled Mono.Cecil (IL inspection)
│   │   └── UDCTester/            # UDC (User-Defined Commands) tester
│   │
│   ├── CompilerTests/     # Compiler test suites
│   ├── Tests/             # Additional test suites
│   ├── Samples/           # Sample projects
│   ├── Common/            # Shared helpers
│   └── Docs/              # Internal documentation
│
├── Artifacts/             # Build output directory
├── docs/                  # GitHub Pages / public documentation
├── ContinuousIntegrationBuild.cmd  # CI bootstrap script
└── RunCompilertests.cmd   # Runs compiler test suite
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

## Building

- **CI entry point**: Run `ContinuousIntegrationBuild.cmd` from the repository root. This bootstraps Roslyn and builds the compiler.
- **Individual subsystems**: Use scripts in `src/` — `buildcompiler.cmd`, `buildrt.cmd`, `buildvs2022.cmd`, etc.
- **Platform**: The CI runs on `windows-2022` with .NET 9 SDK. VS Integration projects target .NET Framework and require a Windows Visual Studio environment.
- **Build output**: Goes to `Artifacts/` at the repository root.

## Testing

- **Compiler tests**: Run `RunCompilertests.cmd` from the repository root (invokes tests under `src/CompilerTests/`).
- **Runtime tests**: Test projects live alongside runtime libraries (e.g., `src/Runtime/XSharp.Core.Tests/`, `src/Runtime/XSharp.RT.Tests/`, etc.). Run with `dotnet test`.
- **Test logs**: CI uploads test logs to `Artifacts/Tests/*.Log`.

## Coding Conventions

- **Line endings**: CRLF (Windows).
- **Indentation**: 4 spaces for code files (`.cs`, `.prg`, `.vh`, `.xs`, `.rc`); 2 spaces for XML/project files.
- **Encoding**: UTF-8 with BOM for code files.
- **Final newline**: Insert for code files; not for XML/XAML files.
- **Trailing whitespace**: Trim for code files.
- See `src/.editorconfig` for complete rules.

## CI / GitHub Actions

The primary workflow is `.github/workflows/continuous-integration.yml`:
- Triggers: nightly schedule (2:00 UTC), pull requests, and manual dispatch.
- Builds the compiler, runs tests, and uploads artifacts (zips, logs, shared DLLs).

## Key Technical Notes

- **Roslyn modifications** are isolated with `#if XSHARP` preprocessor guards. Do not modify Roslyn files outside these guards.
- The compiler uses **ANTLR** for lexing/parsing XSharp syntax.
- Runtime libraries support multiple xBase **dialects** (VO, VFP, XPP, Harbour) via separate assemblies.
- The **macro compiler** exists in two forms: one compiled into the runtime for runtime expression evaluation, and a full version for design-time use.

## Licensing

The project is licensed under the Apache License, Version 2.0. See `src/License.txt`.
