# PropRules.json — Property Conversion Rules

## Purpose

`PropRules.json` controls how VFP object properties are translated into .NET/WinForms
property assignments in the generated X# code.

When a VFP SCX/VCX is read, every object carries a list of raw VFP property/value
pairs (e.g. `Caption = "My Form"`, `Width = 300`). The rules in this file transform
that raw list into the .NET equivalents that are emitted in the designer and form files.

Implementation: `SCXVCXItem.ConvertProperties()` (convert) and
`SCXVCXItem.ApplyPropertiesRules()` (emit); rule sets are built by
`XPorterCtrlForm.BuildControlRules()`.

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
`TextBox`, `Cursor`). Section names and property names are matched **case-insensitively**.

If an object's class has no section (typically a custom class from a VCX), the exporter
climbs the class's inheritance chain through the known custom controls until it finds
a class that has a section. If none is found, only the `Common` rules apply.

Two section names are special:

| Section | Role |
|---------|------|
| `Common` | Rules added to **every control type except `Form`** (see below) |
| `Default` | *(optional, currently absent)* Fallback VFP property values used to complete a template rule when the object itself lacks one of the referenced properties |

---

## Processing Phases

Rules are applied in two distinct phases:

| Phase | Label | What happens |
|-------|-------|--------------|
| 1 | **(C) Convert** | The raw VFP property list is rewritten using the rules |
| 2 | **(A) Apply** | The rewritten list is emitted as X# assignment statements |

Some markers affect only (C), some only (A), and some both.

During (A), every emitted line is prefixed with `SELF:` and, for a child control,
with the control name: `SELF:<ControlName>:<Property> := <Value>`. Dots in a
property key become `:` (the designer does not accept `.`). Keys starting with `_`
are internal and never emitted.

---

## Rule Syntax Reference

### 1. Plain Rename — `"VFPProp": "NetProp"`

*(C)* Renames the property key. The value is carried over unchanged.

```json
"Caption": "Text"
```

`Caption = "My Form"` → `SELF:Text := "My Form"`

User-defined properties (declared in the SCX/VCX, not in the VFP base class) are
**never** renamed or removed by a plain rule, even if their name matches.

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
template string. Placeholder names are matched **case-insensitively**. The property
**keeps its name** (the rule key); only its value is replaced.

```json
"ClientSize": "System.Drawing.Size{<@Width@>, <@Height@>}"
```

`Width = 400`, `Height = 300` →
`SELF:ClientSize := System.Drawing.Size{400, 300}`

The placeholders can reference *any* property on the same object, not only the one
being converted.

Resolution of missing placeholders:

- If **none** of the referenced properties exist on the object, the rule is skipped
  (logged at Verbose level).
- If **some** exist, the others are looked up in the `Default` section. If any are
  still unresolved, the rule is skipped and a Warning is logged.

If the template is wrapped in double quotes and the substituted value already contains
a quote, the template's outer quotes are removed.

---

### 4. Force Quoting — `??…??`

*(C)* Wraps the substituted value in double quotes if it does not already contain
them. Only meaningful inside a template rule (rule 3).

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

### 5. Fixed Value / Constructor Call — value ends with `}` or `)` (no placeholder)

*(C)* The original VFP value is **discarded**, the property keeps its name, and the rule
value is emitted verbatim as the right-hand side of the assignment.

```json
"Margin": "System.Windows.Forms.Padding{0,0,0,0}"
```

Whatever `Margin` was in VFP → `SELF:Margin := System.Windows.Forms.Padding{0,0,0,0}`

A Warning is logged if the value ends with `)` / `}` but has no matching `(` / `{`.

When the value also contains `<@…@>`, it is a template rule (rule 3) instead: the
placeholder values are injected and the VFP value is replaced by the result.

---

### 6. Method Call — value starts with `::` or `:`

*(A)* The property assignment is replaced by a **method call**, using the rule value
(without the leading colons) as the full call including arguments. The call is made
on the object that owns the property: `SELF:` for the form/container itself,
`SELF:<ControlName>:` for a child control.

```json
"DataSession":   ":DataSession(<@DataSession@>)",
"ControlSource": "::SetBinding(<@_CurrentObject_@>, <@ControlSource@>)"
```

`DataSession = 2` on the form → `SELF:DataSession(2)`
`ControlSource = customer.firstname` on `txtName` →
`SELF:txtName:SetBinding(SELF:txtName, customer.firstname)`

> In the current code `::` and `:` produce **identical** output; both forms are used
> in `PropRules.json` (`:` for `DataSession`, `::` for every `ControlSource`).

---

### 7. Force USUAL — `!` on the key or on the value

*(C)+(A)* Forces the emitted value to be wrapped as a `USUAL`. Useful for properties
whose VFP type must be late-bound in X# (typically `Value`).

```json
"Value": "!Value"
```

`Value = 42` → `SELF:Value := __Usual{42}`

A VFP date literal `{...}` is additionally wrapped as a date:
`__Usual{__Date{"..."}}`.

The `!` can be put on the rule key (`"!Value": "Value"`) or at the start of the rule
value (`"Value": "!Value"`, the form used in `PropRules.json`); both mark the property
as force-USUAL.

---

### 8. Prefix Pattern — key starts with `^`

*(C)* Matches every VFP property whose name **starts with** the text after `^`
followed by a number. The digits `99` in both key and value act as a numeric wildcard:
the actual number found in the property name is extracted and substituted, and the
rest of the property name is kept.

```json
"^Button99": "Buttons(99)",
"^Column99": "Column(99)",
"^Page99":   "Pages(99)"
```

`Button3.Width = 80` → `SELF:Buttons(3):Width := 80`
`Column1.Width = 120` → `SELF:Column(1):Width := 120`

A property such as `ButtonCount` is not touched: a digit must follow the prefix.

> Limitation (TODO in `ConvertProperties`): a `^` rule cannot contain `<@…@>`
> placeholders.

---

### 9. Special Placeholder — `<@_CurrentObject_@>`

*(C)* A built-in placeholder that resolves to:

- `SELF` — when the rule is applied to the object itself (form/container)
- `SELF:<ControlName>` — when the rule is applied to a child control

Used with `SetBinding` to wire data-bound controls (see rule 6).

---

## Second Pass — Dotted Child-Property Overrides

A container often carries overrides for its children as dotted keys, e.g.
`Command1.Picture` or `BTN_EXIT.Caption`. After the main pass, rules are applied
again to the **suffix** of each dotted key (`Picture`, `Caption`):

| Rule kind | Handled? |
|-----------|----------|
| Rename / remove | Yes (`!` force-USUAL is propagated) |
| Self-referential single-placeholder template (`??<@Picture@>??`, `VFPTools.ColorFromVFP(<@BorderColor@>)`, `System.Drawing.Icon{??<@Icon@>??}`) | Yes |
| Method call (`:` / `::`) | No — skipped |
| Multi-placeholder templates, fixed constructor values | No — skipped |
| Positional keys (`Column(1).X`, `Buttons(1).X`) | No — skipped |

---

## The `Common` Section

Rules in the `"Common"` block are added to every control type **except `Form`**.
A rule defined in the control's own section always **wins**: a `Common` rule is only
added if the section has no rule with the same key.

Current `Common` keys: `Caption`, `Comment`, `Location`, `Size`, `Top`, `Left`,
`Width`, `Height`, `ForeColor`, `BackColor`, `ControlSource`, `Anchor`, `Picture`,
`OLEDragPicture`, `DragIcon`, `MouseIcon`, `FontName`, `DisabledForeColor`,
`DisabledBackColor`, `RightToLeft`.

---

## Control Sections

The file contains one section per VFP base class that needs property mapping.
A section with no entries (`{}`) is valid: the control gets only the `Common` rules.

| Section | Notable rules |
|---------|---------------|
| `Form` | `Caption`→`Text`, `ClientSize` from Width+Height, removes Top/Left/Width/Height, `DataSession` method call, window style props (`BorderStyle`, `MaxButton`, `MinButton`, `Closable`, `AutoCenter`, `WindowType`, `TitleBar`, …), `Icon`, `Picture`, fonts. Does **not** receive `Common`. |
| `FormSet` | `Caption`, `Location`, `Size`, colors, `WindowType` |
| `CommandButton` | `Caption`, `Style`, `Cancel`/`Default`, picture props, fonts |
| `TextBox` | `ControlSource`→`SetBinding`, `Value` force-USUAL, `Alignment`, `BorderStyle` |
| `EditBox` | `ControlSource`→`SetBinding`, `Scrollbars`→`ScrollBars`, `Value`, `ReadOnly` |
| `Label` | `Caption`, `Size`, `Alignment`, `WordWrap`, fonts |
| `CheckBox` / `OptionButton` | `Caption`, `ControlSource`→`SetBinding`, `Value` force-USUAL, `Style`, picture props |
| `ComboBox` / `ListBox` | `ControlSource`→`SetBinding`, `Value` force-USUAL, `RowSource`, `RowSourceType`, `Style` (+ `MultiSelect`, `Sorted` for ListBox) |
| `Spinner` | `ControlSource`→`SetBinding`, `Value`, high/low values, `Format`, `InputMask` |
| `CommandGroup` / `OptionGroup` | `ControlSource`→`SetBinding`, `^Button99`→`Buttons(99)`, `ButtonCount`, `MemberClass`, `MemberClassLibrary` |
| `Grid` | `^Column99`→`Column(99)`, colors (`HighlightBackColor`, `HighlightForeColor`, `AlternatingRowColor`, `GridLineColor`), layout (`GridLines`, `GridLineWidth`, `RowHeight`, `HeaderHeight`), behavior (`AllowAddNew`, `AllowDelete`, `AllowUpdate`, `AllowHeaderSizing`, `AllowRowSizing`, `AllowAutoColumnFit`), display (`RecordMark`, `DeleteMark`, `HighlightStyle`, `NullDisplay`, `ShowDeleted`, `Themes`), `Scrollbars`→`VFPScrollbars`; `SplitBar` and `Panel` silenced |
| `Column` | `ControlSource`, `Alignment`, `ColumnOrder`, `Sparse`, `Resizable`, `Format`, `InputMask`, `Width`, `SelectOnEntry`; `CurrentControl`, `TextAlign`, `Caption`, `Location`, `Size`, `Anchor`, `Image`, drag/OLE props silenced |
| `header` | `HeaderColor`, `SpecialEffect`, drag/OLE props silenced |
| `PageFrame` | `PageCount`, `ActivePage`, tab props, `^Page99`→`Pages(99)`, fonts |
| `Page` | `Caption`, `PageOrder`, `BackStyle`, `Picture`, fonts |
| `Container` | `BackStyle`, `BorderStyle`, `BorderColor`, `Picture`, fonts |
| `Image` | `Picture`, `Stretch`, `BorderStyle`, `BorderColor`, `BackStyle` |
| `Shape` | `Style`, `Border*`, `Fill*`, `Curvature`, `BackStyle` |
| `Line` | `Border*`, `LineSlant` |
| `Timer` | `Interval`, `Enabled` |
| `ToolBar` | `DockPosition`, `Movable`, fonts |
| `ContextMenu` | `Location`, `Size` |
| `DataEnvironment` | `InitialSelectedAlias`; removes Top/Left/Width/Height |
| `Cursor` | `Alias`, `CursorSource` quoted via `??`, `Database`, `Exclusive`, `Filter`, `Order`, `ReadOnly`; layout props removed |
| `Relation` | `ChildAlias`, `ChildOrder`, `RelationalExpr`, `OneToMany`, `ParentAlias`; layout props removed |
| `Panel` | *(empty)* |

`PropRules.json` itself is the reference for the exact values.

---

## Rule Evaluation Order (within a section)

1. The **section-specific rules** are taken in file order.
2. **Common rules** whose key is not already in the section are appended after them
   (except for `Form`).
3. Rules run **top-to-bottom** over that merged set — a rule can produce a value that
   a later rule transforms further.
4. The second pass (dotted child-property overrides) runs after all rules.
5. A property that has no matching rule is passed through unchanged (original VFP name
   and value are emitted as-is).
6. A property mapped to `""` is silently removed — nothing is emitted.

---

## Quick Reference Card

| Pattern | Where | Effect |
|---------|-------|--------|
| `"VFPProp": "NetProp"` | value | Rename property (user-defined properties excluded) |
| `"VFPProp": ""` | value | Remove property |
| `<@propname@>` | value | Inject another property's value; property keeps its name |
| `??…??` | value | Wrap substituted result in quotes if needed |
| value ends with `)` or `}`, no `<@` | value | Emit the rule value verbatim, discard VFP value |
| value starts with `::` or `:` | value | Emit as method call on the owning object |
| key or value starts with `!` | key / value | Wrap value as `__Usual{…}` |
| key starts with `^` | key | Prefix pattern match; `99` = numeric wildcard |
| `<@_CurrentObject_@>` | value | Resolves to `SELF` or `SELF:<ControlName>` |
| section `Common` | file | Added to every type except `Form`; section rules win |
| section `Default` | file | Fallback values for missing template placeholders |
