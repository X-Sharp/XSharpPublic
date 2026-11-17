# XSharp language *Next Version* Templates package

This package contains some projects templates for .NET 8 and greater for [XSharp language](https://www.xsharp.eu).

## Available Templates in this package  
- xsconsole         : A simple Console application, using the Core dialect.
- xsconsoleFox      : A simple Console application, using the VFP dialect.
- xsconsoleHarbour  : A simple Console application, using the Harbour dialect.
- xsconsoleVO       : A simple Console application, using the Visual Objects dialect.
- xsconsoleXPP      : A simple Console application, using the xBase++ dialect.
- xswebmap          : A RESTful application, using the Core dialect, using WebApplication MapGet/MapPost.
- xswebroute        : A RESTful application, using the Core dialect, using Attributes for routing.
- xswinforms        : A simple Windows Forms application, using the Core dialect.
- xswinformscctrli  : A CustomControl Windows Forms item, using the Core dialect.
- xswinformsctrli   : A UserControl Windows Forms item, using the Core dialect.
- xswinformslib     : A Windows Forms library, using the Core dialect.
- xswpfapp          : A WPF Application, using the Core dialect.
- xswpfcctrllib     : A WPF CustomControl library, using the Core dialect.
- xswpfctrl         : A WPF UserControl library, using the Core dialect.
- xswpflib          : A WPF Library, using the Core dialect.

## Installation 

- Download this package
- Open the download folder
- Run  
    **dotnet new install \<nupkg filename\>**

You can run `dotnet new list` to see the list of all available templates and check the installation.

## Un-Installation 
- Open a terminal
- Run  
    **dotnet new uninstall**
	
You will have a list of all installed packages and the corresponding uninstallation command.

## Usage example
- Create a folder, and name the folder with the name of the desired project  
- Open a terminal in that folder  
- Run `dotnet new xsconsole` to create a new console project in that folder.

In order to build the project, you can run
`dotnet build` in that folder

In order to run (and build if needed) the project, you can run
`dotnet run` in that folder

## Usage with VSCode

The `xsconsole` template comes with some basic setup to build and run the application.  
After running the `dotnet new xsconsole` command, you can type `code .`in order to run VSCode and open that folder.  

You can then modify the source, and run the program using the **Run/Run without debugging** menu.

# Understanding `template.json` for XSharp Project Templates

Each XSharp project template lives under `content\<template-folder>\.template.config\template.json`.
This file is consumed by the .NET Template Engine (`Microsoft.TemplateEngine.Orchestrator.RunnableProjects`),
which powers both the `dotnet new` CLI and the Visual Studio **Create a New Project** dialog.

This document explains the elements used in our templates, using `p_wpfapplication`
(WPF Application) as a reference example.

## Top-Level Fields

| Field | Purpose |
|---|---|
| `$schema` | Points to the JSON schema used for IntelliSense/validation when editing the file. Always `http://json.schemastore.org/template`. |
| `author` | Template author, shown in `dotnet new` listings. Always `"XSharp"`. |
| `classifications` | Tags shown in the VS **Create a New Project** dialog's category filters (e.g., `"Windows"`, `"Desktop"`). |
| `identity` | Globally unique template identifier. Must be unique across all templates installed on a machine (e.g., `"XSharp.WPFApplication.SDK"`). |
| `name` | Friendly display name shown in `dotnet new` and the VS dialog (e.g., `"WPF Application (SDK)"`). |
| `description` | Short description shown under the template name. |
| `SourceName` | A placeholder string used in the template source files (project name, file names, namespace references, etc.) that gets replaced with the actual project name the user enters. |
| `shortName` | The name used to invoke the template from the CLI, e.g. `dotnet new xswpfapplication`. |
| `tags` | Free-form metadata. We use `language` (`"X#"`), `type` (`"project"` or `"item"`), and `icon`/`is-icon-path` to associate a `template.png` icon with the template. |
| `preferNameDirectory` | When `true`, creates a new subdirectory named after the project when generating files. |
| `defaultName` | The name used when the user doesn't supply one (e.g., `"WpfApplication"`). |

## The `symbols` Section

Symbols define template parameters: values that are substituted into the generated source files.
Each symbol has a **type**, which determines how its value is obtained.

### `"type": "parameter"`

A user-supplied value, settable via `dotnet new <template> --<SymbolName> <value>` on the CLI.

"TargetFrameworkOverride": { "type": "parameter", "description": "Overrides the target framework", "replaces": "TargetFrameworkOverride", "datatype": "string", "defaultvalue": "", "displayName": "Target framework override" }


| Property | Purpose |
|---|---|
| `type` | `"parameter"` — a settable value. |
| `description` | Help text shown in `dotnet new <template> -?`. |
| `datatype` | `"string"`, `"bool"`, `"choice"`, etc. |
| `replaces` | The literal placeholder string in the template source files that gets replaced with this symbol's value. |
| `defaultvalue` | Value used if the user doesn't supply one. |
| `displayName` | Friendly label for CLI help output / generic tooling. **Note:** Visual Studio's native **Create a New Project** dialog does not render arbitrary parameter symbols — only a small set of symbol names it specially recognizes (see below). `displayName` is primarily useful for the `dotnet new` CLI experience. |

#### `"datatype": "choice"`

Used for the `Framework` symbol, which lets the user pick a target .NET version:

> **Important:** `Framework` is a **reserved/special-cased symbol name** in Visual Studio's
> New Project dialog. VS renders its own native target-framework picker control for any symbol
> named `Framework`, ignoring custom `displayName` text for that control. Do not rename this
> symbol if you want VS's native framework-selection UI to continue to work.

### `"type": "derived"`

A value computed automatically from another symbol, with no user input and no UI presence.
Used for `DefaultNamespace`, which is derived from the project name: