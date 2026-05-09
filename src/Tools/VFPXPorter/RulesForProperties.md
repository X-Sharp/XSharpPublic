# PropRules.json — Property Conversion Rules

## Purpose

`PropRules.json` controls how VFP object properties are translated into .NET/WinForms
property assignments in the generated X# code.

When a VFP SCX/VCX is read, every object carries a list of raw VFP property/value
pairs (e.g. `Caption = "My Form"`, `Width = 300`). The rules in this file transform
that raw list into the .NET equivalents that are emitted in the designer and form files.

---

## File Format

```json
{
  "<VFPControlType>": {
    "<VFPPropertyName>": "<Rule>",
    ...
  },
  ...
}
```

`<VFPControlType>` matches the VFP `BaseClass` name (e.g. `Form`, `CommandButton`,
`TextBox`, `Cursor`). Matching is **case-insensitive**.

A special section named `"Common"` applies its rules to **all control types except
`Form`**. Common rules are merged with the type-specific rules before processing.

Rules are applied **top-down**: a rule can produce a new property name/value that a
later rule then transforms again.

---

## Processing Phases

Rules are applied in two distinct phases:

| Phase | Label | What happens |
|-------|-------|--------------|
| 1 | **(C) Convert** | The raw VFP property list is rewritten using the rules |
| 2 | **(A) Apply** | The rewritten list is emitted as X# assignment statements |

Some markers affect only (C), some only (A), and some both.

---

## Rule Syntax Reference

### 1. Plain Rename — `"VFPProp": "NetProp"`

*(C)* Renames the property key. The value is carried over unchanged.

```json
"Caption": "Text"
```

`Caption = "My Form"` → `SELF:Text := "My Form"`

---

### 2. Remove — `"VFPProp": ""`

*(C)* Removes the property entirely. Nothing is emitted for it.

```json
"DoCreate": "",
"Top": "",
"Left": ""
```

> **Important:** if you need the value of a property elsewhere (e.g. in a template),
> use it in a template rule *before* the removal rule.

---

### 3. Template with Placeholders — `<@propname@>`

*(C)* Builds a new value by substituting one or more VFP property values into a
template string. Placeholder names are matched **case-insensitively**.

```json
"ClientSize": "System.Drawing.Size{<@Width@>, <@Height@>}"
```

`Width = 400`, `Height = 300` →
`SELF:ClientSize := System.Drawing.Size{400, 300}`

The placeholders can reference *any* property on the same object, not only the one
being converted.

---

### 4. Force Quoting — `??…??`

*(C)* Wraps the substituted value in double quotes if it does not already contain
them.

```json
"CursorSource": "??<@CursorSource@>??"
"FontName":     "??<@FontName@>??"
```

`..\data\customer.dbf` → `"..\data\customer.dbf"`  
`Arial` → `"Arial"`  
`"Arial"` → `"Arial"` *(already quoted — no change)*

Can be combined with other template content:

```json
"Icon": "System.Drawing.Icon{??<@Icon@>??}"
```

`myapp.ico` → `System.Drawing.Icon{"myapp.ico"}`

---

### 5. Object / Constructor Call — value ends with `}` or `)`

*(C)* The original VFP value is **discarded** and the rule value is emitted verbatim
as the right-hand side of the assignment.

```json
"Margin": "System.Windows.Forms.Padding{0,0,0,0}"
```

Whatever `Margin` was in VFP → `SELF:Margin := System.Windows.Forms.Padding{0,0,0,0}`

When combined with `<@…@>` the placeholder values are injected first, then the result
is emitted as a constructor call:

```json
"ClientSize": "System.Drawing.Size{<@Width@>, <@Height@>}"
```

---

### 6. Method Call (form scope) — value starts with `::`

*(A)* The property assignment is replaced by a **method call on SELF**, using the rule
value as the full call including arguments.

```json
"DataSession": ":DataSession(<@DataSession@>)"
```

`DataSession = 2` → `SELF:DataSession(2)` *(called on the form itself)*

---

### 7. Method Call (child scope) — value starts with `:`

*(A)* Similar to `::`, but the call may be applied to a **child control** rather than
SELF directly, depending on context.

```json
"ControlSource": ":SetBinding(<@_CurrentObject_@>, <@ControlSource@>)"
```

Emits a `SetBinding(…)` call targeting the current control being processed.

---

### 8. Force USUAL — property name starts with `!`

*(C)+(A)* Forces the property value to be cast as `USUAL` in the generated code.
Useful for properties whose VFP type must be late-bound in X#.

```json
"Value": "!Value"
```

`Value = 42` → `SELF:Value := (USUAL) 42`

When the *rule value* (right-hand side) starts with `!`:

*(A)* The generated assignment wraps the emitted value in a `(USUAL)` cast.

---

### 9. Prefix Pattern — property name starts with `^`

*(C)* Matches any VFP property whose name **starts with** the text after `^`. The
digits `99` in both key and value act as a numeric wildcard: the actual number found
in the property name is extracted and substituted.

```json
"^Option99": "Buttons(99)",
"^Button99": "Buttons(99)"
```

`Option1.Caption = "Yes"` → `SELF:Buttons(1):Caption := "Yes"`  
`Button3.Width = 80` → `SELF:Buttons(3):Width := 80`

---

### 10. Special Placeholder — `<@_CurrentObject_@>`

*(C)* A built-in placeholder that resolves to:

- `SELF` — when the rule is applied to a top-level item (the form itself, a control
  with no container parent)
- `SELF:<ControlName>` — when the rule is applied inside a container child

Used primarily with `SetBinding` to wire data-bound controls:

```json
"ControlSource": "::SetBinding(<@_CurrentObject_@>, <@ControlSource@>)"
```

Inside a `TextBox` named `txtName`:
→ `SELF:SetBinding(SELF:txtName, "customer.firstname")`

---

## The `Common` Section

Rules in the `"Common"` block are **merged** into every control type **except `Form`**.
Use it for properties that have the same translation across all controls:

```json
"Common": {
  "Margin":    "System.Windows.Forms.Padding{0,0,0,0}",
  "FontBold":  "<@FontBold@>",
  "FontName":  "??<@FontName@>??",
  "FontSize":  "<@FontSize@>"
}
```

---

## Control Sections

The file contains one section per VFP base class that needs property mapping.
Sections with no entries (empty `{}`) are valid — they suppress the "unknown control"
warning without mapping any properties.

| Section | Notable rules |
|---------|---------------|
| `Form` | `Caption`→`Text`, `ClientSize` from Width+Height, removes Top/Left/Width/Height |
| `CommandButton` | `Caption`→`Text`, font properties |
| `TextBox` | `ControlSource`→`SetBinding`, `TextAlign`→`VFPTools.TextAlignmentConvert`, `Value` forced USUAL |
| `EditBox` | `ControlSource`→`SetBinding`, `Scrollbars`→`VFPScrollbars` |
| `Label` | `Caption`→`Text`, `Size` from Width+Height, font properties |
| `CheckBox` | `Caption`→`Text`, `ControlSource`→`SetBinding`, `Value` forced USUAL |
| `ComboBox` | `ControlSource`→`SetBinding`, `Value` forced USUAL, `Style`→`DropDownStyle` |
| `ListBox` | `ControlSource`→`SetBinding`, `Value` forced USUAL |
| `Spinner` | `ControlSource`→`SetBinding`, `Value` forced USUAL |
| `CommandGroup` | `ControlSource`→`SetBinding`, `^Button99`→`Buttons(99)` prefix pattern |
| `OptionGroup` | `ControlSource`→`SetBinding`, `^Option99`→`Buttons(99)` prefix pattern |
| `Grid` | Column-level properties (ColumnCount, RecordSource, RecordSourceType) |
| `DataEnvironment` | Removes Top/Left/Width/Height |
| `Cursor` | `CursorSource` quoted via `??`, `Location`/Width/Height removed |
| `Relation` | ChildAlias, ChildOrder, RelationalExpr, OneToMany, ParentAlias |
| `Image` | *(empty — no mappings needed)* |
| `Shape` | *(empty)* |
| `Line` | *(empty)* |
| `Timer` | *(empty)* |
| `Container` | *(empty)* |
| `Page` | *(empty)* |
| `PageFrame` | *(empty)* |

---

## Rule Evaluation Order (within a section)

1. **Common rules** are merged first (except for `Form`).
2. **Section-specific rules** are then applied on top.
3. Rules run **top-to-bottom** within the merged set — a rule can produce a value that
   a later rule transforms further.
4. A property that has no matching rule is passed through unchanged (original VFP name
   and value are emitted as-is).
5. A property mapped to `""` is silently removed — nothing is emitted.

---

## Quick Reference Card

| Pattern | Where | Effect |
|---------|-------|--------|
| `"VFPProp": "NetProp"` | key | Rename property |
| `"VFPProp": ""` | key | Remove property |
| `<@propname@>` | value | Inject other property's value |
| `??…??` | value | Wrap substituted result in quotes if needed |
| value ends with `)` or `}` | value | Emit as constructor/method call, discard VFP value |
| value starts with `::` | value | Emit as method call on SELF (form scope) |
| value starts with `:` | value | Emit as method call on current child (control scope) |
| key starts with `!` | key | Force value to USUAL type |
| value starts with `!` | value | Cast emitted value to USUAL |
| key starts with `^` | key | Prefix pattern match; `99` = numeric wildcard |
| `<@_CurrentObject_@>` | value | Resolves to `SELF` or `SELF:<ControlName>` |
