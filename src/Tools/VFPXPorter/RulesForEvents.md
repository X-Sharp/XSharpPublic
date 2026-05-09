# EventRules.json — Event Conversion Rules

## Purpose

`EventRules.json` controls how VFP event handlers are translated into .NET/WinForms
event wiring and X# method declarations in the generated code.

When a VFP SCX/VCX is read, each object carries a list of event code blocks
(e.g. `Click`, `GotFocus`, `InteractiveChange`). The rules in this file translate
those VFP event names into the correct .NET event subscription or VFP.UI property
assignment, and define the method prototype for the generated handler.

---

## File Format

```json
{
  "<VFPControlType>": {
    "<VFPEventName>": [ "<NetTarget>", "<MethodPrototypeSuffix>", "<EventHandlerType>" ]
  }
}
```

Each rule is a **3-element array**:

| Index | Name | Description |
|-------|------|-------------|
| `[1]` | `NetTarget` | The .NET event name, property name, or call target (see patterns below) |
| `[2]` | `MethodPrototypeSuffix` | Parameter list and return type appended to the generated method name |
| `[3]` | `EventHandlerType` | The delegate type used for `+=` subscriptions (e.g. `System.EventHandler`) |

`<VFPControlType>` matches the VFP `BaseClass` name (case-insensitive).

A special section `"Common"` applies its rules to **all control types**. When a
control also has a type-specific section, type-specific rules take precedence over
`Common` for the same event name.

---

## Rule Patterns

The behaviour is determined by the **suffix of `NetTarget`** (element `[1]`).

---

### 1. VFP-style Property Assignment — `NetTarget` ends with `:=`

The most common pattern for VFP events. The control's VFP.UI support property is
set to the **name of the handler method as a string**. The VFP.UI runtime calls it
at the right moment, forwarding VFP-compatible arguments.

```json
"Click":    [ "vfpClick:=",    "", "" ],
"GotFocus": [ "vfpGotFocus:=", "", "" ],
"When":     [ "vfpWhen:=",     "", "" ]
```

**Generated in designer (InitializeComponent):**
```xsharp
SELF:Command1:vfpClick := "Command1_Click"
```

**Generated method prototype (in form file):**
```xsharp
METHOD Command1_Click() AS USUAL
```

The method name is built from the owner control name + `_` + event name (see
[Handler Naming](#handler-naming) below). The prototype suffix `data[2]` is empty,
so the method gets `() AS USUAL` by default.

---

### 2. Direct .NET Event Mapping — `NetTarget` is a plain .NET event name

Maps a VFP event to a true WinForms `+=` event subscription. The third element
specifies the delegate type.

```json
"Timer":             [ "Tick",        "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ],
"InteractiveChange": [ "TextChanged", "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ]
```

**Generated in designer (InitializeComponent):**
```xsharp
SELF:timer1:Tick += System.EventHandler{ SELF, @Form1.Timer1_Timer() }
```

**Generated method prototype (in form file):**
```xsharp
METHOD Timer1_Timer(sender AS OBJECT, e AS System.EventArgs) AS VOID
```

The delegate type from `[3]` is used for the `+=` subscription. The prototype
suffix from `[2]` is appended to the method name.

---

### 3. VFP.UI Override Object — `NetTarget` ends with `{}`

Sets a property to a **VFP.UI override object** that intercepts a .NET event and
reroutes it with VFP-compatible parameters. The third element is the class name
of the override object.

```json
"Refresh": [ "VFPRefresh{}", "() AS USUAL CLIPPER", "VFPOverride" ]
```

**Generated in designer:**
```xsharp
SELF:ctrl:VFPRefresh := VFPOverride{ SELF, "Control_Refresh" }
```

**Generated method prototype:**
```xsharp
METHOD Control_Refresh() AS USUAL CLIPPER
```

The override object is constructed with two arguments: `SELF` (the form) and the
handler method name as a string.

---

### 4. Direct Method Call — `NetTarget` ends with `()`

The event is not wired as an event handler at all. Instead, the method is called
**directly** at an appropriate point (e.g. during `Init`). The third element, if
not empty, specifies the call class.

```json
"Load": [ "vfpLoad:=", "", "" ]
```

*(In current usage this pattern is mainly expressed via the `:=` form; the `()`
form is retained for special cases like `Init` that must be called directly.)*

---

## Handler Naming

The generated method name is assembled from several components, controlled by
`XPorterSettings`:

```
[FormName_] + [ControlName_] + [EventName]
```

| Setting | Effect |
|---------|--------|
| `PrefixEvent = TRUE` | Prepend the form class name to event handlers on the form itself |
| `KeepFoxProEventName = TRUE` | Use the VFP event name (e.g. `Click`) rather than the .NET target name |
| `RemoveSet = TRUE` | Strip a leading `Set_` from the handler name |

**Example** — `CommandButton` named `Command1`, event `Click`, rule target `vfpClick:=`:

| Settings | Generated method name |
|----------|-----------------------|
| `PrefixEvent=false`, `KeepFoxProEventName=true` | `Command1_Click` |
| `PrefixEvent=true`, `KeepFoxProEventName=true` | `Form1_Command1_Click` |
| `PrefixEvent=false`, `KeepFoxProEventName=false` | `Command1_vfpClick` |
| `PrefixEvent=false`, `KeepFoxProEventName=false`, `RemoveSet=true` | `Command1_vfpClick` *(no Set_ to remove)* |

Any `.` (dot) in the assembled name is replaced by `_` automatically.

---

## The `Common` Section

Rules in `"Common"` are merged into every control type. Type-specific rules for the
same event name **override** the common rule.

```json
"Common": {
  "Click":    [ "vfpClick:=",    "", "" ],
  "GotFocus": [ "vfpGotFocus:=", "", "" ],
  ...
},
"textbox": {
  "InteractiveChange": [ "vfpInteractiveChange:=", "", "" ]
}
```

Here `textbox` overrides the common `InteractiveChange` rule (which maps to
`TextChanged`) with its own VFP-style property assignment.

---

## `GenerateOnlyHandledEvent` Setting

When `XPorterSettings.GenerateOnlyHandledEvent = TRUE` (the default), only events
that appear in `EventRules.json` produce generated output. Event blocks found in
the SCX that have no matching rule are silently skipped.

When `FALSE`, unmatched events are still emitted using a fallback: the VFP event
name is kept as-is, the method gets `() AS USUAL` as its prototype, and the event
is marked as unhandled in the output.

---

## Current Rules Summary

### Common (all controls)

| VFP Event | Net Target | Pattern |
|-----------|-----------|---------|
| `Click` | `vfpClick` | `:=` |
| `RightClick` | `vfpRightClick` | `:=` |
| `DblClick` | `vfpDblClick` | `:=` |
| `Destroy` | `vfpDestroy` | `:=` |
| `GotFocus` | `vfpGotFocus` | `:=` |
| `Init` | `vfpInit` | `:=` |
| `InteractiveChange` | `TextChanged` | .NET event (`System.EventHandler`) |
| `LostFocus` | `vfpLostFocus` | `:=` |
| `MouseEnter` | `vfpMouseEnter` | `:=` |
| `MouseLeave` | `vfpMouseLeave` | `:=` |
| `MouseUp` | `vfpMouseUp` | `:=` |
| `MouseDown` | `vfpMouseDown` | `:=` |
| `MouseMove` | `vfpMouseMove` | `:=` |
| `Refresh` | `vfpRefresh` | `:=` |
| `Valid` | `vfpValid` | `:=` |
| `When` | `vfpWhen` | `:=` |
| `KeyPress` | `vfpKeyPress` | `:=` |

### Timer

| VFP Event | Net Target | Pattern |
|-----------|-----------|---------|
| `Timer` | `Tick` | .NET event (`System.EventHandler`) |

### TextBox (overrides Common)

| VFP Event | Net Target | Pattern |
|-----------|-----------|---------|
| `InteractiveChange` | `vfpInteractiveChange` | `:=` *(overrides Common's TextChanged)* |

### Grid

| VFP Event | Net Target | Pattern |
|-----------|-----------|---------|
| `AfterRowColChange` | `vfpAfterRowColChange` | `:=` |
| `OnColumnHeaderClick` | `vfpColumnHeaderClick` | `:=` |

### Form

| VFP Event | Net Target | Pattern |
|-----------|-----------|---------|
| `QueryUnload` | `vfpQueryUnload` | `:=` |
| `Load` | `vfpLoad` | `:=` |

---

## Quick Reference Card

| `NetTarget` suffix | Pattern | Designer output | Method prototype |
|-------------------|---------|-----------------|-----------------|
| ends with `:=` | VFP.UI property | `SELF:ctrl:vfpXxx := "Handler"` | `METHOD Handler() AS USUAL` |
| plain name | .NET event | `SELF:ctrl:Event += DelegateType{ SELF, @Class.Handler() }` | `METHOD Handler(params) AS type` |
| ends with `{}` | VFP.UI override object | `SELF:ctrl:VFPXxx := VFPOverride{ SELF, "Handler" }` | `METHOD Handler() AS USUAL CLIPPER` |
| ends with `()` | Direct method call | *(called at init time, no `+=`)* | `METHOD Handler() AS USUAL` |
