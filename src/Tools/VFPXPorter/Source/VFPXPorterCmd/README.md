# VFPXPorterCmd

Command-line interface for **VFPXPorter** — a tool that converts Visual FoxPro (VFP)
forms, class libraries, and full projects to X# / WinForms code.

The actual conversion engine lives in **VFPXPorterLib**. VFPXPorterCmd is a thin
wrapper around that library, intended for scripted and batch use. The same library
is also used by the GUI tool (**VFPXPorter**).

---

## Usage

```
VFPXPorterCmd -o:<output> [-f:<file> ...] [-p:<project>] [-b] [-l:<logfile>] [conversion options]
```

Paths may be absolute or relative to the current directory. Wrap paths containing
spaces in double quotes: `-f:"C:\My App\myform.scx"`.

### Input / output

| Argument | Description |
|----------|-------------|
| `-f:<path>` | SCX (form) or VCX (class library) to convert. Repeatable. |
| `-p:<path>` | PJX (VFP project) to convert entirely (forms, libraries, menus, programs). |
| `-o:<path>` | Output folder. **Created automatically** if it does not exist. |
| `-b` | Back up the source structure to XML files before converting. |
| `-l:<path>` | Write log output to a file in addition to the console. |

### Conversion options

| Argument | Default | Description |
|----------|---------|-------------|
| `-keepOriginal` | on | Keep original VFP code as comments in the output. |
| `-noKeepOriginal` | — | Suppress the original code comments. |
| `-storeInFolders` | off | Organize output into Forms / Libs / Menus / Code subfolders. |
| `-outputType:<type>` | `WindowsExe` | Target project type: `WindowsExe`, `ClassLibrary`, or `Console`. |
| `-modifier:<mod>` | `PUBLIC` | Access modifier for generated fields: `PUBLIC`, `PROTECTED`, or `PRIVATE`. |

Exit codes: `0` = success, `1` = error.

### Examples

Convert a single form:
```
VFPXPorterCmd -f:"C:\MyApp\myform.scx" -o:"C:\Output"
```

Convert a full VFP project with backup and logging:
```
VFPXPorterCmd -p:"C:\MyApp\myapp.pjx" -o:"C:\Output" -b -l:convert.log
```

Convert several standalone files in one run:
```
VFPXPorterCmd -f:"forms\customer.scx" -f:"libs\common.vcx" -o:"C:\Output"
```

---

## What is generated

### From a single SCX / VCX file (`-f`)

| File | Content |
|------|---------|
| `<FormName>.prg` | Form class — constructor, event handlers, user-defined methods |
| `<FormName>.designer.prg` | Designer class — control declarations and InitComponent() |
| `Backup\` *(with `-b`)* | XML snapshot of the SCX/VCX internal structure |

### From a PJX project file (`-p`)

Everything above, for each form and class library in the project, plus:

| File | Content |
|------|---------|
| `<ProjectName>.xsproj` | X# project file referencing all generated sources |
| `<ProjectName>.sln` | Visual Studio solution |
| `VFPStart.prg` | Application entry point (`Main`) |
| `VFPXPorter.xh` | Header file with support-library imports |
| Menu files *(if any)* | X# menu classes |

---

## Deployment requirements

VFPXPorterCmd needs the following alongside the executable:

```
VFPXPorterCmd.exe
VFPXPorterLib.dll
Data\
  PropRules.json
  EventRules.json
  TypeConvert.json
  Statements.json
  VFP2WinForms.json
  ColorProperties.json
  Templates\
    Designer\   (prefix.prg, starttype.prg, inittype.prg, endtype.prg)
    Form\       (prefix.prg, starttype.prg, inittype.prg, endtype.prg)
    SingleFile\ (prefix*.prg, starttype*.prg, inittype*.prg, endtype*.prg)
    Tools\      (VFPXPorter.xh, VFPCmd.xh, Helpers.prg, XSharp.VFP.UI.dll)
    Others\     (VFPStart.prg)
    Menu\       (MenuContainer.prg)
```

---

## Current state

| Feature | Status | Notes |
|---------|--------|-------|
| SCX form export | Working | Single file via `-f` |
| VCX library export | Working | Single file via `-f` |
| PJX project export | Working | Full project via `-p` |
| Menu (MNX) export | Working | Via project export only |
| Report (FRX) export | Not implemented | Planned |
| Backup to XML | Working | `-b` flag |
| File logging | Working | `-l` flag |
| Exit codes | Working | 0 = OK, 1 = error |
| Auto-create output folder | Working | |
| Batch (multiple `-f`) | Working | Repeat `-f` as needed |
| Settings overrides | Working | `-keepOriginal`, `-convertHandlers`, `-storeInFolders`, `-outputType`, `-modifier` |

---

## Planned / Not yet implemented

- **Report (FRX) export** — conversion of VFP reports to a .NET reporting format.
- **Config file** — load settings from a JSON/INI file per project instead of
  specifying them on every command line.

---

## Settings defaults (XPorterSettings)

These are the conversion options currently used. They can only be changed by
modifying the library or the generated code by hand.

| Setting | Default | Meaning |
|---------|---------|---------|
| `KeepOriginal` | `TRUE` | Keep original VFP code as comments |
| `ConvertThisObject` | `TRUE` | Replace `this` with `thisObject` |
| `ConvertStatement` | `TRUE` | Convert VFP statements to method calls |
| `RemoveSet` | `TRUE` | Remove `Set_` prefix in event handler names |
| `PrefixClassFile` | `TRUE` | Prefix output files with the form/library name |
| `StoreInFolders` | `FALSE` | Organize output into Forms/Libs/Menus/Code subfolders |
| `LibInSubFolder` | `TRUE` | Place library files in a subfolder |
| `Modifier` | `PUBLIC` | Default access modifier for generated fields |
| `OutputType` | `WindowsExe` | Target project type (WindowsExe / ClassLibrary / Console) |
