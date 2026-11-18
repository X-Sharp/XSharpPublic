# Templates and Placeholders

## Overview

Templates are static X# code fragments with **`<@placeholder@>`** markers that the
generator replaces at export time. Together they form the skeleton of every generated
file. Templates live under `Data\Templates\` next to the executable
(source: `Source\VFPXPorterLib\Data\Templates\`).

```
Data\Templates\
  Designer\        — designer partial class (two-file mode)
  Form\            — form partial class (two-file mode)
  SingleFile\      — combined class (one-file mode, container and non-container variants)
  ReportListener\  — classes derived from ReportListener (no WinForms dependency)
  Menu\            — VFP menu export
  Others\          — application entry point
  Tools\           — support files copied into every project
```

Designer, Form, SingleFile and ReportListener contain up to four files assembled in order:

| File | Role |
|------|------|
| `prefix.prg` | USING statements, file header |
| `StartType.prg` | Class declaration opening, field declarations |
| `InitType.prg` | Constructor and methods body |
| `EndType.prg` | Class closing (`END CLASS`) |

The non-container SingleFile variant uses `prefixNotContainer.prg`,
`StartTypeNotContainer.prg`, `InitTypeNotContainer.prg` and `EndTypeNotContainer.prg`.

---

## Placeholder Substitution

Placeholders use the syntax `<@name@>`. Substitution is done by
`TemplateHelper.ReplaceAndValidate()`, in a single pass over the template:

- placeholder names are matched **case-insensitively** (`<@FormName@>` = `<@formName@>`);
- a placeholder the code does not provide (typo, or a name unknown to the exporter) is
  replaced by **empty text** and logged as a **Warning** naming the template — the
  export goes on, but the generated code may then not compile;
- a value provided by the code that the template does not use is silently ignored;
- inserted values are never re-scanned, so converted user code containing `<@` is safe.

So a placeholder can be **removed** from a customised template (the corresponding
generated code is simply dropped); a misspelled one shows up as a Warning in the log.

---

## Generation Modes

### Two-file mode (Designer + Form)

Used for SCX/VCX items that are **containers** (forms, custom containers, pages). Produces
two partial classes that compile together:

- **`<Name>.designer.prg`** — built from `Designer\` templates. Contains field
  declarations, `Dispose()` and `InitializeComponent()`.
- **`<Name>.prg`** — built from `Form\` templates. Contains the constructor,
  event handlers, data environment setup, and user-defined methods.

### Single-file mode (SingleFile)

Used for **library class items** (VCX objects, or controls exported standalone).
Everything lands in one `.prg` file built from `SingleFile\` templates.

Two variants exist within SingleFile:

- **Container variant** (`prefix`, `StartType`, `InitType`, `EndType`) — for objects that
  host child controls. Includes `InitializeComponent()`, `InitContainers()`,
  `InitGrids()`, and `SetDataEnvironment()`.
- **Non-container variant** (`prefixNotContainer`, `StartTypeNotContainer`,
  `InitTypeNotContainer`, `EndTypeNotContainer`) — for simple objects with no child
  controls. Omits the container-specific infrastructure.

### ReportListener mode

Items whose class or base class is `ReportListener` use the `ReportListener\` templates
instead (no WinForms base, no designer).

---

## Placeholder Reference

### Designer templates (`Designer\`)

#### `StartType.prg`

| Placeholder | Replaced with |
|-------------|---------------|
| `<@formName@>` | The X# class name derived from the SCX object name |
| `<@superName@>` | The .NET base class name (from `TypeConvert.json`) |
| `<@childsDeclaration@>` | `PUBLIC fieldName AS TypeName` lines for every child control |

#### `InitType.prg`

| Placeholder | Replaced with |
|-------------|---------------|
| `<@childsInstantiate@>` | `SELF:ctrl := TypeName{}` instantiation lines for every child control |
| `<@childsInitialize@>` | Property assignments for every child control (from `PropRules.json`) |
| `<@addChildsToParent@>` | `SELF:Controls:Add(SELF:ctrl)` lines |
| `<@formProps@>` | Property assignments for the form/container itself (user-defined properties excluded) |

---

### Form templates (`Form\`)

#### `StartType.prg`

| Placeholder | Replaced with |
|-------------|---------------|
| `<@cursorAliases@>` | `// CURSOR alias1, alias2, …` comment listing the DataEnvironment cursor aliases (empty when there are none) |
| `<@formName@>` | The X# class name |
| `<@superName@>` | The .NET base class name |
| `<@dataenvironment@>` | Field declarations for the DataEnvironment and its cursors |
| `<@childsDeclaration@>` | Child control field declarations |

#### `InitType.prg`

The constructor calls, in order: `PreInitializeComponent()`, `SetDataEnvironment()`,
`InitializeComponent()`, `InitContainers()`, `InitGrids()`, then the user-defined
property initialisation and the optional `DoBindings()`.

| Placeholder | Replaced with |
|-------------|---------------|
| `<@userdefProps@>` | User-defined property assignments (properties declared in the SCX but not in the base class) |
| `<@DoBindings@>` | `DoBindings()` when the form has a DataEnvironment, empty otherwise |
| `<@InitContainers@>` | Event handler wiring for child elements of custom controls |
| `<@InitGrids@>` | Column property assignments for Grid controls |
| `<@setdataenvironment@>` | Body of `SetDataEnvironment()`: DataEnvironment + Cursor + Relation initialisation |
| `<@EventHandlers@>` | All generated `METHOD` blocks for event handlers and user-defined methods |

#### `EndType.prg`

Only `END CLASS`. No placeholders.

---

### SingleFile templates (`SingleFile\`)

#### Container variant (`StartType` / `InitType` / `EndType`)

Combines Designer and Form into one class.

**`StartType.prg`**

| Placeholder | Replaced with |
|-------------|---------------|
| `<@cursorAliases@>` | DataEnvironment cursor aliases comment |
| `<@formName@>` | Class name |
| `<@superName@>` | Base class name |
| `<@childsDeclaration@>` | Child control field declarations |

The code also provides `dataenvironment`, which the current template does not use
(ignored).

**`InitType.prg`**

| Placeholder | Replaced with |
|-------------|---------------|
| `<@childsInstantiate@>` | Control instantiation lines |
| `<@childsInitialize@>` | Control property assignments |
| `<@addChildsToParent@>` | `Controls:Add` lines |
| `<@formProps@>` | Container-level property assignments |
| `<@userdefProps@>` | User-defined property assignments |
| `<@DoBindings@>` | `DoBindings()` when a DataEnvironment exists |
| `<@InitContainers@>` | Event wiring for custom control children |
| `<@InitGrids@>` | Grid column property assignments |
| `<@EventHandlers@>` | Generated METHOD blocks |
| `<@setdataenvironment@>` | DataEnvironment initialisation block |

#### Non-container variant

`StartTypeNotContainer.prg`: `<@formName@>`, `<@superName@>`, `<@childsDeclaration@>`.

`InitTypeNotContainer.prg` (no `InitContainers`, `InitGrids`, `SetDataEnvironment`):

| Placeholder | Replaced with |
|-------------|---------------|
| `<@childsInstantiate@>` | Instantiation lines |
| `<@childsInitialize@>` | Property assignments |
| `<@addChildsToParent@>` | `Controls:Add` lines |
| `<@formProps@>` | Object-level properties |
| `<@userdefProps@>` | User-defined property assignments |
| `<@EventHandlers@>` | METHOD blocks |

#### `InitTypeBinding.prg` (appended to a SingleFile class when needed)

When the exported class needs data binding, this fixed block is written **after**
`InitType` and before `EndType`:

```xsharp
PROPERTY BindingDefinition AS System.Collections.Generic.Dictionary<Control,STRING> AUTO

METHOD SetBinding( sender AS Control, bindingInfo AS STRING ) AS VOID
    IF SELF:BindingDefinition == NULL
        SELF:BindingDefinition := System.Collections.Generic.Dictionary<Control,STRING>{}
    ENDIF
    SELF:BindingDefinition:Add( sender, bindingInfo )
```

No placeholders — content is fixed.

---

### ReportListener templates (`ReportListener\`)

| File | Placeholders |
|------|--------------|
| `StartType.prg` | `<@formName@>`, `<@superName@>`, `<@childsDeclaration@>` |
| `InitType.prg` | `<@formProps@>`, `<@userdefProps@>`, `<@EventHandlers@>` |

---

### Menu template (`Menu\MenuContainer.prg`)

| Placeholder | Replaced with |
|-------------|---------------|
| `<@MenuName@>` | Class name derived from the MNX file name |
| `<@MenuInit@>` | `AddPad`, `AddPopup`, `AddBar` calls and `vfpClick` wiring for all menu items, inside `Init()` |
| `<@MenuCode@>` | One `METHOD name() AS USUAL STRICT` block per action bar, containing converted handler code |

The template generates **two classes** from a single MNX:

- `<MenuName>` — inherits `XSharp.VFP.UI.Menu`, contains all menu logic.
- `<MenuName>.MPR` — inherits `<MenuName>`, preserves the VFP convention of
  running a menu via `DO <menu>.MPR`.

`Menu.ThisForm` is set by `Activate(oForm)` so handler code translated from
`THISFORM.xxx` resolves correctly at runtime.

---

### Others (`Others\VFPStart.prg`)

The application entry point (`FUNCTION Start`), written once per project export.
It creates `_Screen` and `_vfp`, sets the start-up target on `_Screen`, then runs
`Application.Run(_Screen)`. Exactly one of the three placeholders is filled, depending
on the PJX main item:

| Placeholder | Filled when the main item is… | Value |
|-------------|-------------------------------|-------|
| `<@startcode@>` | a `.prg` | `<PrgName>()` — evaluated by `MainWindow` as `StartFunction` |
| `<@startform@>` | a form | `ClassNamePrefix` + form name — launched as `StartForm` |
| `<@startmenu@>` | a `.mnx` | `ClassNamePrefix` + menu name — launched as `StartMenu` |

Fixed globals emitted unconditionally:

```xsharp
GLOBAL _Screen AS XSharp.VFP.UI.MainWindow
GLOBAL _vfp   AS XSharp.VFP.UI.Application
GLOBAL _MSYSMENU AS XSharp.VFP.UI.Menu
```

---

### Tools folder (`Tools\`)

Every `*.prg`, `*.xh` and `*.dll` in this folder is **copied verbatim** into every
exported project (into an `XSharp\` subfolder when *Store in folders* is on) — no
template processing occurs. They are also added to the generated `.xsproj`.

| File | Purpose |
|------|---------|
| `VFPXPorter.xh` | Main header: `#include`s `VFPCmd.xh` and `VFPXMenu.xh` |
| `VFPCmd.xh` | VFP command compatibility UDCs (unsupported commands become `#warning`) |
| `VFPXMenu.xh` | UDCs for procedural menu commands (`DEFINE MENU/PAD/POPUP/BAR`, `ON PAD/BAR/SELECTION`, `ACTIVATE`, `RELEASE`, `SET SYSMENU`, …) mapped to `XSharp.VFP.UI` `MenuCommands` |
| `VFPCreateObject.prg` | `CreateObject` support (runtime fallback for class-name prefix) |
| `Helpers.prg` | Small runtime helpers used by generated code |
| `XSharp.VFP.UI.dll` | The VFP.UI runtime library itself |

---

## Placeholder Quick Reference

| Placeholder | Templates | Content |
|-------------|-----------|---------|
| `<@formName@>` | All StartType | X# class name |
| `<@superName@>` | All StartType | .NET base class name |
| `<@childsDeclaration@>` | All StartType | `PUBLIC ctrl AS Type` field lines |
| `<@cursorAliases@>` | Form/StartType, SingleFile/StartType | Cursor alias comment |
| `<@dataenvironment@>` | Form/StartType | DataEnvironment and cursor field declarations |
| `<@childsInstantiate@>` | Designer/InitType, SingleFile/InitType* | `SELF:ctrl := Type{}` lines |
| `<@childsInitialize@>` | Designer/InitType, SingleFile/InitType* | Control property assignment lines |
| `<@addChildsToParent@>` | Designer/InitType, SingleFile/InitType* | `SELF:Controls:Add(...)` lines |
| `<@formProps@>` | Designer/InitType, SingleFile/InitType*, ReportListener/InitType | Form/container property lines |
| `<@userdefProps@>` | Form/InitType, SingleFile/InitType*, ReportListener/InitType | User-defined property assignments |
| `<@DoBindings@>` | Form/InitType, SingleFile/InitType | `DoBindings()` or empty |
| `<@InitContainers@>` | Form/InitType, SingleFile/InitType | Custom control child event wiring |
| `<@InitGrids@>` | Form/InitType, SingleFile/InitType | Grid column property lines |
| `<@setdataenvironment@>` | Form/InitType, SingleFile/InitType | DataEnvironment + Cursors init block |
| `<@EventHandlers@>` | Form/InitType, SingleFile/InitType*, ReportListener/InitType | All generated METHOD blocks |
| `<@MenuName@>` | Menu/MenuContainer | Menu class name |
| `<@MenuInit@>` | Menu/MenuContainer | Menu structure initialisation |
| `<@MenuCode@>` | Menu/MenuContainer | Menu action handler methods |
| `<@startcode@>` | Others/VFPStart | Start-up function call (`.prg` main) |
| `<@startform@>` | Others/VFPStart | Start-up form class name |
| `<@startmenu@>` | Others/VFPStart | Start-up menu class name |

`*` = both the container and the non-container (`…NotContainer`) variant.
