# VFPXPorterCmd

Command-line interface for **VFPXPorter** — a tool that converts Visual FoxPro (VFP)
forms, class libraries, and full projects to X# / WinForms code.

The actual conversion engine lives in **VFPXPorterLib**. VFPXPorterCmd is a thin
wrapper around that library, intended for scripted and batch use. The same library
is also used by the GUI tool (**VFPXPorter**).

---

## Usage

```
VFPXPorterCmd -o:"<output>" [-f:"<file>" ...] [-p:"<project>"] [options]
```

Paths may be absolute or relative to the current directory. Wrap paths containing
spaces in double quotes: `-p:"C:\My App\myapp.pjx"`.

A log file is written automatically to `VFPXPorter.log` in the output folder.
Use `-l:"<path>"` to write it elsewhere instead.

### Input / output

| Argument | Description |
|----------|-------------|
| `-f:"<path>"` | SCX (form) or VCX (class library) to convert. Repeatable. |
| `-p:"<path>"` | PJX (VFP project) to convert entirely (forms, libraries, menus, programs). |
| `-o:"<path>"` | Output folder. **Created automatically** if it does not exist. |
| `-b` | Back up the source structure to XML files before converting. |
| `-l:"<path>"` | Write log to a specific file instead of the default location. |
| `-dataFolder:"<path>"` | Override the Data folder (templates, rules JSON files, etc.). |
| `-outputType:<type>` | Target project type: `WindowsExe` (default), `ClassLibrary`, or `Console`. |

### Project options

All boolean flags accept a `-no` prefix to invert them (e.g. `-noStoreInFolders`).

| Argument | Default | Description |
|----------|---------|-------------|
| `-storeInFolders` | **on** | Organize output into Forms / Libs / Menus / Code subfolders. |
| `-emptyFolder` | on | Empty the output folder before exporting. |
| `-libInSubFolder` | on | Place library files in a subfolder of the output. |
| `-addLibraryNamespace` | on | Add the library namespace to `VFPXPorter.xh`. |
| `-ignoreErrors` | on | Keep exporting after non-fatal errors. |
| `-folderNames:"<map>"` | `Forms=Forms;Libs=Libs;...` | Custom subfolder name mapping. |

### Code conversion options

| Argument | Default | Description |
|----------|---------|-------------|
| `-keepOriginal` | on | Keep original VFP code as comments in the output. |
| `-convertThisObject` | on | Convert `This` / `ThisForm` references. |
| `-convertStatement` | on | Convert VFP statements to method calls. |
| `-convertStatementOnlyIfLast` | **on** | Only convert a statement if it is the last token on the line. |
| `-prefixClassFile` | **off** | Prefix output file names with the form / library name. |
| `-prefixEvent` | **on** | Prefix event methods with the form name. |
| `-keepFoxProEventName` | on | Keep the FoxPro event name in generated event handlers. |
| `-generateOnlyHandledEvent` | **on** | Only emit event handlers listed in `EventRules.json`. |
| `-removeSet` | on | Remove the `Set_` prefix from event handler names. |
| `-nameUDF` | off | Use the XPorter-built name for user-defined methods. |
| `-modifier:<mod>` | `PUBLIC` | Access modifier for generated fields: `PUBLIC`, `PROTECTED`, or `PRIVATE`. |

Exit codes: `0` = success, `1` = error.

### Examples

Convert a full VFP project:
```
VFPXPorterCmd -p:"C:\MyApp\myapp.pjx" -o:"C:\Output"
```

Convert a full project with backup and a custom log:
```
VFPXPorterCmd -p:"C:\MyApp\myapp.pjx" -o:"C:\Output" -b -l:"C:\Logs\export.log"
```

Convert a single form:
```
VFPXPorterCmd -f:"C:\MyApp\myform.scx" -o:"C:\Output"
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
| File logging | Working | Auto `VFPXPorter.log` or `-l` flag |
| Exit codes | Working | 0 = OK, 1 = error |
| Auto-create output folder | Working | |
| Batch (multiple `-f`) | Working | Repeat `-f` as needed |
| Settings overrides | Working | All `XPorterSettings` fields exposed as switches |

---

## Planned / Not yet implemented

- **Report (FRX) export** — conversion of VFP reports to a .NET reporting format.
- **Config file** — load settings from a JSON file per project instead of
  specifying them on every command line.
