# Understanding `template.json` for XSharp Project Templates

Each XSharp project template lives under `content\<template-folder>\.template.config\template.json`.
This file is consumed by the .NET Template Engine (`Microsoft.TemplateEngine.Orchestrator.RunnableProjects`),
which powers both the `dotnet new` CLI and the Visual Studio **Create a New Project** dialog.

This document explains the elements used in our templates, using `p_wpfapplication`
(WPF Application) as a reference example.

## Top-Level Fields

```
 "$schema": "http://json.schemastore.org/template",
  "author": "XSharp",
  "classifications": [
    "Windows",
    "Android",
    "Linux",
    "MacOS",
    "Library"
  ],
  "identity": "XSharp.ClassLibrary.SDK",
  "name": "Class Library Core Dialect without Runtime (SDK)",
  "description": "A SDK Style project for creating a X# class library (.dll) for the Core dialect with no dependencies on the runtime",
  "SourceName": "XSharp.ClassLibrary",
  "shortName": "xsclasslibrary",
  "tags": {
    "language": "X#",
    "type": "project",
    "icon": "template.png",
    "is-icon-path": "true"
  },
  "preferNameDirectory": true,
  "defaultName": "ClassLibrary",

```

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

```
"TargetFrameworkOverride": {
      "type": "parameter",
      "description": "Overrides the target framework",
      "replaces": "TargetFrameworkOverride",
      "datatype": "string",
      "defaultvalue": "",
      "displayName": "Target framework override"
    },

```


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

```
 "Framework": {
      "type": "parameter",
      "description": "Target .NET version",
      "displayName": "Target framework",
      "datatype": "choice",
      "choices": [
        {
          "choice": "net8.0",
          "description": "Target .NET 8"
        },
        {
          "choice": "net9.0",
          "description": "Target .NET 9"
        },
        {
          "choice": "net10.0",
          "description": "Target .NET 10"
        },
        {
          "choice": "net11.0",
          "description": "Target .NET 11"
        }
      ],
      "replaces": "net8.0",
      "defaultvalue": "net8.0"
    }
```



> **Important:** `Framework` is a **reserved/special-cased symbol name** in Visual Studio's
> New Project dialog. VS renders its own native target-framework picker control for any symbol
> named `Framework`, ignoring custom `displayName` text for that control. Do not rename this
> symbol if you want VS's native framework-selection UI to continue to work.

### `"type": "derived"`

A value computed automatically from another symbol, with no user input and no UI presence.
Used for `DefaultNamespace`, which is derived from the project name:


```
 "DefaultNamespace": {
      "type": "derived",
      "valueSource": "name",
      "valueTransform": "safe_namespace",
      "replaces": "Company.Namespace1"
    }
```


| Property | Purpose |
|---|---|
| `valueSource` | The symbol to derive from. `"name"` refers to the built-in symbol holding the project name entered by the user. |
| `valueTransform` | Name of a transform defined in the top-level `forms` section (see below) that converts the source value into the final value. |
| `replaces` | Same as for parameter symbols — the placeholder text replaced in source files. |

## The `forms` Section

`forms` defines reusable value transforms that can be referenced by `valueTransform` (on derived
symbols) or `forms` (on parameter symbols, for additional replacement variants).

```
"forms": {
    "safe_namespace": {
      "identifier": "replace",
      "pattern": "[^A-Za-z0-9_.]",
      "replacement": "_"
    }
  }

  ```

| Property | Purpose |
|---|---|
| `identifier` | The transform kind. `"replace"` performs a regex replace. |
| `pattern` | Regex pattern matching characters to replace. |
| `replacement` | The replacement string. |

This particular form strips any character that isn't a letter, digit, underscore, or dot from the
project name, producing a value safe to use as a namespace.

## Practical Notes for XSharp Templates

- **`Framework`** — reserved name, gets native VS UI treatment; keep this name unless you intend to lose that integration.
- **`TargetFrameworkOverride`** — a free-form escape hatch, only usable via CLI (`dotnet new ... --TargetFrameworkOverride net12.0`) since VS doesn't surface custom parameter symbols in its dialog.
- **`DefaultNamespace`** — derived automatically from the project name via the `safe_namespace` form; requires no user input and isn't shown in any UI, CLI or VS.
- VS's **Create a New Project** dialog only renders a small, hardcoded set of recognized symbols (chiefly `Framework`). Any other custom `parameter` symbol is only accessible through the `dotnet new` CLI, not through Visual Studio's dialog.
- For full, up-to-date parameter documentation (independent of VS dialog limitations), run:

``dotnet new <shortName> -?`
