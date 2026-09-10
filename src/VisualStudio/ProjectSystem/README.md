# XSharp CPS Project System

This folder contains the **CPS-based (Common Project System)** implementation of the XSharp project system for Visual Studio 2022 and later.

## Overview

| Path | Purpose |
|---|---|
| `XSharp.ProjectSystem2022.csproj` | Project file (net472, VS2022 SDK references) |
| `XSharpProjectCapabilities.cs` | CPS capability constants (`XSharp`, `Managed`, `MultipleTargetFrameworks`, …) |
| `XSharpProjectCapabilitiesProvider.cs` | MEF export that advertises capabilities to CPS |
| `XSharpProjectType.cs` | Binds the `.xsproj` extension and project GUID to this CPS project system |
| `XSharpProjectTreePropertiesProvider.cs` | Custom icons for `.xsfrm`, `.xsmnu`, `.prg`, `.xaml`, etc. |
| `XSharpProjectBuildProvider.cs` | Build-property integration + up-to-date check stub |
| `XSharpProjectProperties.cs` | Strongly-typed wrappers around MSBuild project properties |
| `XSharpTargetFrameworkProvider.cs` | Multi-targeting support (VS2022+ only — reads `<TargetFrameworks>`) |
| `XSharpCodeDomIntegration.cs` | Bridge to the existing XSharp CodeDom provider for Windows Forms designer |
| `PropertyPages/*.cs` | CPS `IPageMetadata` exports for Application, Build, and Debug pages |
| `Rules/XSharp.xaml` | MSBuild property rules (XAML format) surfaced in the project designer |
| `Rules/XSharpMultiTarget.xaml` | `TargetFrameworks` rule for multi-targeting (VS2022+ only) |
| `XSharpCpsPackage.cs` | VS AsyncPackage that registers CPS components |
| `XSharpCpsPackage.pkgdef` | Registry entries that activate CPS for `.xsproj` in VS2022+ |

## VS Version Strategy

| Feature | VS2019 | VS2022 / VS2026 |
|---|---|---|
| Project system | **MPF** (`ProjectBase` + `ProjectPackage`) | **CPS** (this assembly) |
| Multi-targeting | ❌ not supported | ✅ via `<TargetFrameworks>` |
| VO binary editors (`.xsfrm`, `.xsmnu`, …) | ✅ | ✅ |
| CodeDom / WinForms designer | ✅ | ✅ |
| Project designer pages | ✅ AppDesigner | ✅ AppDesigner + CPS rules |

The MPF code in `ProjectBase` and `ProjectPackage` is **not modified**.  The CPS package activates only in VS2022+ via the `ProjectSystemPackage` registry entry in `XSharpCpsPackage.pkgdef`.

## Multi-Targeting (VS2022+ only)

SDK-style `.xsproj` files can use `<TargetFrameworks>` to target multiple frameworks simultaneously:

```xml
<Project Sdk="XSharp.Net.Sdk">
  <PropertyGroup>
    <TargetFrameworks>net48;net6.0-windows</TargetFrameworks>
    <Dialect>Core</Dialect>
  </PropertyGroup>
</Project>
```

> **Note:** The VS2019 / MPF path does **not** support multi-targeting.
> Use a single `<TargetFramework>` value when VS2019 compatibility is required.

## Custom Editors

The VO binary file editors (`.xsfrm`, `.xsmnu`, `.xsdbs`, `.xsfs`) are implemented in the `ProjectPackage` assembly and registered both there (for MPF) and in `XSharpCpsPackage.pkgdef` (for CPS).  No code duplication is required — the factory implementations are shared.

## CodeDom / Windows Forms

`XSharpCodeDomIntegration` locates `VSXsharpCodeDomProvider` in the `ProjectPackage` assembly at run-time and exposes it through the MEF `ICodeDomProviderIntegration` interface so the Windows Forms designer can generate XSharp source code in the CPS context.

## Building

This project is included in `src/VSIntegration2022.sln`.  It is **not** included in `src/VSIntegration.sln` (VS2019).

```
msbuild src/VSIntegration2022.sln /p:Configuration=Release
```
