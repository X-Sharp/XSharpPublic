# Templates and Placeholders

## Overview

Templates are static X# code fragments with **`<@placeholder@>`** markers that the
generator replaces at export time. Together they form the skeleton of every generated
file. Templates live under `Data\Templates\` next to the executable.

```
Data\Templates\
  Designer\        — designer partial class (two-file mode)
  Form\            — form partial class (two-file mode)
  SingleFile\      — combined class (one-file mode, container objects)
  SingleFile\      — combined class (one-file mode, non-container objects)
  Menu\            — VFP menu export
  Others\          — application entry point
  Tools\           — support files copied into every project
```

Each folder contains up to four files assembled in order:

| File | Role |
|------|------|
| `prefix.prg` | USING statements, file header |
| `StartType.prg` | Class declaration opening, field declarations |
| `InitType.prg` | Constructor and methods body |
| `EndType.prg` | Class closing (`END CLASS`) |

---

## Generation Modes

### Two-file mode (Designer + Form)

Used for SCX/VCX items that are **containers** (forms, custom containers). Produces
two partial classes that compile together:

- **`<Name>.designer.prg`** — built from `Designer\` templates. Contains field
  declarations, `InitializeComponent()`, and `Dispose()`.
- **`<Name>.prg`** — built from `Form\` templates. Contains the constructor,
  event handlers, data environment setup, and user-defined methods.

### Single-file mode (SingleFile)

Used for **library class items** (VCX non-container objects, or controls exported
standalone). Everything lands in one `.prg` file built from `SingleFile\` templates.

Two variants exist within SingleFile:

- **Container variant** (`StartType`, `InitType`, `EndType`) — for objects that
  host child controls. Includes `InitializeComponent()`, `InitContainers()`,
  `InitGrids()`, and `SetDataEnvironment()`.
- **Non-container variant** (`StartTypeNotContainer`, `InitTypeNotContainer`,
  `EndTypeNotContainer`) — for simple objects with no child controls. Omits
  the container-specific infrastructure.

---

## Placeholder Reference

Placeholders use the syntax `<@name@>` (case-insensitive). The generator substitutes
each marker with generated X# code before writing the output file.

---

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
| `<@formProps@>` | Property assignments for the form/container itself |
| `<@userdefProps@>` | User-defined property assignments (properties declared in the SCX but not in the base class) |

---

### Form templates (`Form\`)

#### `StartType.prg`

| Placeholder | Replaced with |
|-------------|---------------|
| `<@formName@>` | The X# class name |
| `<@superName@>` | The .NET base class name |
| `<@dataenvironment@>` | `PUBLIC Cursor1 AS DbCursor` field declarations for DataEnvironment cursors |
| `<@childsDeclaration@>` | Child control field declarations (same as Designer, used when form is not split) |

#### `InitType.prg`

| Placeholder | Replaced with |
|-------------|---------------|
| `<@userdefProps@>` | User-defined property init calls inserted after `SetDataEnvironment()` in the constructor |
| `<@InitContainers@>` | Event handler wiring for child elements of custom controls |
| `<@InitGrids@>` | Column property assignments for Grid controls |
| `<@setdataenvironment@>` | Full `DataEnvironment` + `Cursor` initialisation block, `DoBindings()` call |
| `<@EventHandlers@>` | All generated `METHOD` blocks for event handlers and user-defined methods |

---

### SingleFile templates (`SingleFile\`)

#### Container variant (`StartType` / `InitType` / `EndType`)

Combines everything from Designer and Form into one class. Placeholders are a union
of both sets:

**`StartType.prg`**

| Placeholder | Replaced with |
|-------------|---------------|
| `<@formName@>` | Class name |
| `<@superName@>` | Base class name |
| `<@childsDeclaration@>` | Child control field declarations |

**`InitType.prg`**

| Placeholder | Replaced with |
|-------------|---------------|
| `<@childsInstantiate@>` | Control instantiation lines |
| `<@childsInitialize@>` | Control property assignments |
| `<@addChildsToParent@>` | `Controls:Add` lines |
| `<@formProps@>` | Container-level property assignments |
| `<@userdefProps@>` | User-defined property init calls in the constructor |
| `<@InitContainers@>` | Event wiring for custom control children |
| `<@InitGrids@>` | Grid column property assignments |
| `<@EventHandlers@>` | Generated METHOD blocks |
| `<@setdataenvironment@>` | DataEnvironment initialisation block |

#### Non-container variant (`StartTypeNotContainer` / `InitTypeNotContainer` / `EndTypeNotContainer`)

Same placeholders as the container variant but the constructor is simpler (no
`InitContainers`, no `InitGrids`, no `SetDataEnvironment`):

| Placeholder | Replaced with |
|-------------|---------------|
| `<@formName@>` | Class name |
| `<@superName@>` | Base class name |
| `<@childsDeclaration@>` | Field declarations |
| `<@childsInstantiate@>` | Instantiation lines |
| `<@childsInitialize@>` | Property assignments |
| `<@addChildsToParent@>` | `Controls:Add` lines |
| `<@formProps@>` | Container-level properties |
| `<@userdefProps@>` | User-defined property calls |
| `<@EventHandlers@>` | METHOD blocks |

#### `InitTypeBinding.prg` (injected into SingleFile container)

Adds data-binding infrastructure to a SingleFile container class. Injected as a
block inside the class body (not a standalone file):

```xsharp
PROPERTY BindingDefinition AS Dictionary<Control, STRING> AUTO

METHOD SetBinding( sender AS Control, bindingInfo AS STRING ) AS VOID
    IF SELF:BindingDefinition == NULL
        SELF:BindingDefinition := Dictionary<Control, STRING>{}
    ENDIF
    SELF:BindingDefinition:Add( sender, bindingInfo )
```

No placeholders — content is fixed.

---

### Menu template (`Menu\MenuContainer.prg`)

| Placeholder | Replaced with |
|-------------|---------------|
| `<@MenuName@>` | Class name derived from the MNX file name |
| `<@MenuInit@>` | `AddPad`, `AddPopup`, `AddBar` calls and `vfpClick` wiring for all menu items |
| `<@MenuCode@>` | One `METHOD name() AS USUAL STRICT` block per action bar, containing converted handler code |

The template generates **two classes** from a single MNX:

- `<MenuName>` — inherits `XSharp.VFP.UI.Menu`, contains all menu logic.
- `<MenuName>.MPR` — inherits `<MenuName>`, preserves the VFP convention of
  running a menu via `DO <menu>.MPR`.

`Menu.ThisForm` is set by `Activate(oForm)` so handler code translated from
`THISFORM.xxx` resolves correctly at runtime.

---

### Others (`Others\VFPStart.prg`)

The application entry point, copied once per project export.

| Placeholder | Replaced with |
|-------------|---------------|
| `<@startcode@>` | The converted body of the VFP startup program (the `.prg` marked as the main program in the PJX) |

Fixed globals emitted unconditionally:

```xsharp
GLOBAL _Screen AS XSharp.VFP.UI.MainWindow
GLOBAL _vfp   AS XSharp.VFP.UI.Application
GLOBAL _MSYSMENU AS XSharp.VFP.UI.Menu
```

---

### Tools folder (`Tools\`)

These files are **copied verbatim** into every exported project — no template
processing occurs. They are also added to the generated `.xsproj`.

| File | Purpose |
|------|---------|
| `VFPXPorter.xh` | Header file with `#include` directives for the VFP.UI support library |
| `VFPCmd.xh` | Additional VFP command compatibility definitions |
| `Helpers.prg` | Small runtime helpers used by generated code |
| `XSharp.VFP.UI.dll` | The VFP.UI runtime library itself |

---

## Placeholder Quick Reference

| Placeholder | Templates | Content |
|-------------|-----------|---------|
| `<@formName@>` | All StartType | X# class name |
| `<@superName@>` | All StartType | .NET base class name |
| `<@childsDeclaration@>` | All StartType | `PUBLIC ctrl AS Type` field lines |
| `<@dataenvironment@>` | Form/StartType | Cursor field declarations |
| `<@childsInstantiate@>` | Designer/InitType, SingleFile/InitType | `SELF:ctrl := Type{}` lines |
| `<@childsInitialize@>` | Designer/InitType, SingleFile/InitType | Control property assignment lines |
| `<@addChildsToParent@>` | Designer/InitType, SingleFile/InitType | `SELF:Controls:Add(...)` lines |
| `<@formProps@>` | Designer/InitType, SingleFile/InitType | Form/container property lines |
| `<@userdefProps@>` | Form/InitType, SingleFile/InitType | User-defined property init calls |
| `<@InitContainers@>` | Form/InitType, SingleFile/InitType | Custom control child event wiring |
| `<@InitGrids@>` | Form/InitType, SingleFile/InitType | Grid column property lines |
| `<@setdataenvironment@>` | Form/InitType, SingleFile/InitType | DataEnvironment + Cursors init block |
| `<@EventHandlers@>` | Form/InitType, SingleFile/InitType | All generated METHOD blocks |
| `<@MenuName@>` | Menu/MenuContainer | Menu class name |
| `<@MenuInit@>` | Menu/MenuContainer | Menu structure initialisation |
| `<@MenuCode@>` | Menu/MenuContainer | Menu action handler methods |
| `<@startcode@>` | Others/VFPStart | Startup program body |
