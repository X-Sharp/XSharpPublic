// Column.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.



USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Column class.
	/// </summary>
	PARTIAL CLASS Column INHERIT System.Windows.Forms.DataGridViewTextBoxColumn IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		CONSTRUCTOR(  )
            SUPER()
            SELF:Width := 75
			RETURN

			// ControlSource: maps to DataGridViewColumn.DataPropertyName so data binding works.
		// In VFP a column's ControlSource is the field name from the cursor/alias.
		PROPERTY ControlSource AS STRING
			GET
				RETURN SELF:DataPropertyName
			END GET
			SET
				LOCAL fieldName := IIF( VALUE == NULL, "", VALUE ) AS STRING
				IF fieldName:IndexOf(".") > 0
					fieldName := fieldName:Substring(fieldName:IndexOf(".") + 1)
				ENDIF
				SELF:DataPropertyName := fieldName
			END SET
		END PROPERTY

		PROPERTY Font AS System.Drawing.Font GET SELF:DefaultCellStyle:Font SET SELF:DefaultCellStyle:Font := VALUE

		// ── BackColor / ForeColor ─────────────────────────────────────────────
		// Map to DefaultCellStyle so the colour applies to all cells in this column.

		PROPERTY BackColor AS System.Drawing.Color
			GET
				RETURN SELF:DefaultCellStyle:BackColor
			END GET
			SET
				SELF:DefaultCellStyle:BackColor := VALUE
			END SET
		END PROPERTY

		PROPERTY ForeColor AS System.Drawing.Color
			GET
				RETURN SELF:DefaultCellStyle:ForeColor
			END GET
			SET
				SELF:DefaultCellStyle:ForeColor := VALUE
			END SET
		END PROPERTY

		// ── DynamicBackColor / DynamicForeColor ──────────────────────────────
		// Store the VFP expression string and compile it into a codeblock on assignment.
		// OnCellFormatting evaluates the codeblock for each cell and applies the result.

		PRIVATE _dynBackColorStr AS STRING
		PRIVATE _dynBackColorCB  AS CODEBLOCK
		PRIVATE _dynForeColorStr AS STRING
		PRIVATE _dynForeColorCB  AS CODEBLOCK

		PROPERTY DynamicBackColor AS STRING
			GET
				RETURN SELF:_dynBackColorStr
			END GET
			SET
				SELF:_dynBackColorStr := VALUE
				SELF:_dynBackColorCB  := IIF( String.IsNullOrEmpty(VALUE), NULL_OBJECT, MCompile("{|| " + VALUE + "}") )
			END SET
		END PROPERTY

		PROPERTY DynamicForeColor AS STRING
			GET
				RETURN SELF:_dynForeColorStr
			END GET
			SET
				SELF:_dynForeColorStr := VALUE
				SELF:_dynForeColorCB  := IIF( String.IsNullOrEmpty(VALUE), NULL_OBJECT, MCompile("{|| " + VALUE + "}") )
			END SET
		END PROPERTY

		// ── Alignment ────────────────────────────────────────────────────────
		// VFP Alignment: 0=Left, 1=Right, 2=Center — maps to DataGridViewContentAlignment.

		PROPERTY Alignment AS LONG
			GET
				DO CASE
				CASE SELF:DefaultCellStyle:Alignment == DataGridViewContentAlignment.MiddleRight
					RETURN 1
				CASE SELF:DefaultCellStyle:Alignment == DataGridViewContentAlignment.MiddleCenter
					RETURN 2
				OTHERWISE
					RETURN 0
				END CASE
			END GET
			SET
				DO CASE
				CASE VALUE == 1
					SELF:DefaultCellStyle:Alignment := DataGridViewContentAlignment.MiddleRight
				CASE VALUE == 2
					SELF:DefaultCellStyle:Alignment := DataGridViewContentAlignment.MiddleCenter
				OTHERWISE
					SELF:DefaultCellStyle:Alignment := DataGridViewContentAlignment.MiddleLeft
				END CASE
			END SET
		END PROPERTY

		// ── ColumnOrder ───────────────────────────────────────────────────────
		// DisplayIndex can only be set after the column is added to a DataGridView.
		// Store the requested value and apply it in OnDataGridViewChanged() if the
		// column is not yet in a grid.

		PRIVATE _pendingColumnOrder AS INT
		PRIVATE _hasPendingColumnOrder AS LOGIC

		PROPERTY ColumnOrder AS LONG
			GET
				RETURN SELF:DisplayIndex + 1  // VFP is 1-based
			END GET
			SET
				IF SELF:DataGridView != NULL_OBJECT
					SELF:DisplayIndex := VALUE - 1
				ELSE
					SELF:_pendingColumnOrder := VALUE
					SELF:_hasPendingColumnOrder := TRUE
				ENDIF
			END SET
		END PROPERTY

		PROPERTY Header AS Header
		GET
			IF SELF:HeaderCell IS Header
				RETURN (Header)SELF:HeaderCell
			ENDIF
			VAR oHeader := Header{ SELF:HeaderCell }
			SELF:HeaderCell := oHeader
			RETURN oHeader
		END GET
		SET
			SELF:HeaderCell := VALUE
		END SET
		END PROPERTY

		// ── Format / InputMask ───────────────────────────────────────────────
		// Store the raw VFP format/mask strings.
		// Where a direct .NET DataGridViewCellStyle.Format mapping exists it is applied
		// immediately; otherwise a CellFormatting handler applies the conversion at
		// paint time once the column is attached to a DataGridView.

		PRIVATE _vfpFormat    AS STRING
		PRIVATE _vfpInputMask AS STRING

		PROPERTY Format AS STRING
			GET
				RETURN SELF:_vfpFormat
			END GET
			SET
				SELF:_vfpFormat := VALUE
				SELF:ApplyVFPFormat()
			END SET
		END PROPERTY

		PROPERTY InputMask AS STRING
			GET
				RETURN SELF:_vfpInputMask
			END GET
			SET
				SELF:_vfpInputMask := VALUE
				SELF:ApplyVFPFormat()
			END SET
		END PROPERTY

		// Convert VFP Format/InputMask to a .NET CellStyle.Format string where possible.
		PRIVATE METHOD ApplyVFPFormat() AS VOID STRICT
			VAR fmt := IIF( String.IsNullOrEmpty(SELF:_vfpFormat), "", SELF:_vfpFormat:ToUpper():Trim() )
			VAR mask := IIF( String.IsNullOrEmpty(SELF:_vfpInputMask), "", SELF:_vfpInputMask:Trim() )
			// Date / DateTime
			IF fmt:Contains("@D") .OR. fmt:Contains("@DL") .OR. fmt:Contains("@DS")
				SELF:DefaultCellStyle:Format := "d"  // short date
				RETURN
			ENDIF
			IF fmt:Contains("@DT")
				SELF:DefaultCellStyle:Format := "g"  // short date+time
				RETURN
			ENDIF
			// Currency
			IF fmt:Contains("$") .OR. mask:Contains("$")
				SELF:DefaultCellStyle:Format := "C2"
				RETURN
			ENDIF
			// Numeric picture mask: count digits after decimal point
			VAR decSep := mask:IndexOf(".")
			IF decSep >= 0
				VAR decimals := 0
				FOR VAR i := decSep + 1 TO mask:Length - 1
					VAR ch := mask:Substring(i, 1)
					IF ch == "9" .OR. ch == "#" .OR. ch == "*"
						decimals++
					ENDIF
				NEXT
				SELF:DefaultCellStyle:Format := "N" + decimals:ToString()
				RETURN
			ENDIF
			// Pure integer mask
			IF mask:Length > 0 .AND. mask:Replace("9",""):Replace("#",""):Replace("*",""):Replace(",",""):Trim():Length == 0
				SELF:DefaultCellStyle:Format := "N0"
				RETURN
			ENDIF
			// Uppercase (@!) — handled by CellFormatting; no .NET equivalent format string
			// Everything else: clear any previously set format
			IF String.IsNullOrEmpty(fmt) .AND. String.IsNullOrEmpty(mask)
				SELF:DefaultCellStyle:Format := ""
			ENDIF

		// Hook CellFormatting on the parent DataGridView to handle @! uppercase etc.
		PROTECTED OVERRIDE METHOD OnDataGridViewChanged() AS VOID STRICT
			SUPER:OnDataGridViewChanged()
			IF SELF:DataGridView != NULL_OBJECT
				// Apply deferred ColumnOrder (DisplayIndex requires the column to be in a grid)
				IF SELF:_hasPendingColumnOrder
					SELF:DisplayIndex := SELF:_pendingColumnOrder - 1
					SELF:_hasPendingColumnOrder := FALSE
				ENDIF
				// Wire CellFormatting for VFP format/mask support
				SELF:DataGridView:CellFormatting += DataGridViewCellFormattingEventHandler{ SELF, @OnCellFormatting() }
				// Wire column-header mouse events so Header vfp* callbacks fire
				SELF:DataGridView:ColumnHeaderMouseClick       += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnColumnHeaderMouseClick() }
				SELF:DataGridView:ColumnHeaderMouseDoubleClick += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnColumnHeaderMouseDblClick() }
				SELF:DataGridView:CellMouseEnter               += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @OnCellMouseEnter() }
				SELF:DataGridView:CellMouseLeave               += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @OnCellMouseLeave() }
				// Wire column cell events for vfp* callbacks
				SELF:DataGridView:CellMouseClick  += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnCellMouseClick() }
				SELF:DataGridView:CellDoubleClick += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @OnCellDoubleClick() }
				SELF:DataGridView:CellEnter       += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @OnCellEnter() }
				SELF:DataGridView:CellLeave       += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @OnCellLeave() }
				SELF:DataGridView:CellMouseDown   += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnCellMouseDownInner() }
				SELF:DataGridView:CellMouseUp     += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnCellMouseUpInner() }
				SELF:DataGridView:CellMouseMove   += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnCellMouseMoveInner() }
				SELF:DataGridView:CellValidating  += System.Windows.Forms.DataGridViewCellValidatingEventHandler{ SELF, @OnCellValidating() }
				SELF:DataGridView:CellBeginEdit   += System.Windows.Forms.DataGridViewCellCancelEventHandler{ SELF, @OnCellBeginEdit() }
				SELF:DataGridView:KeyPress        += System.Windows.Forms.KeyPressEventHandler{ SELF, @OnGridKeyPress() }
				SELF:DataGridView:EditingControlShowing += System.Windows.Forms.DataGridViewEditingControlShowingEventHandler{ SELF, @OnEditingControlShowing() }
				// Fire Init the first time this column is attached to a grid
				IF !SELF:_initFired
					SELF:_initFired := TRUE
					SELF:_VFPInit?:Call()
				ENDIF
			ENDIF

		PRIVATE METHOD _GetHeader() AS Header
			IF SELF:HeaderCell IS Header VAR hdr
				RETURN hdr
			ENDIF
			RETURN NULL_OBJECT

		PRIVATE METHOD OnColumnHeaderMouseClick( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs ) AS VOID STRICT
			IF e:ColumnIndex == SELF:Index
				VAR hdr := SELF:_GetHeader()
				IF hdr != NULL_OBJECT
					IF e:Button == System.Windows.Forms.MouseButtons.Right
						hdr:FireRightClick()
					ELSE
						hdr:FireClick()
					ENDIF
				ENDIF
			ENDIF

		PRIVATE METHOD OnColumnHeaderMouseDblClick( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs ) AS VOID STRICT
			IF e:ColumnIndex == SELF:Index
				SELF:_GetHeader()?:FireDblClick()
			ENDIF


		// CellMouseEnter/Leave: RowIndex == -1 is the header row, >= 0 is a data cell.
		PRIVATE METHOD OnCellMouseEnter( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs ) AS VOID STRICT
			IF e:ColumnIndex == SELF:Index
				IF e:RowIndex == -1
					SELF:_GetHeader()?:FireMouseEnter()
				ELSE
					SELF:_VFPMouseEnter?:Call()
				ENDIF
			ENDIF

		PRIVATE METHOD OnCellMouseLeave( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs ) AS VOID STRICT
			IF e:ColumnIndex == SELF:Index
				IF e:RowIndex == -1
					SELF:_GetHeader()?:FireMouseLeave()
				ELSE
					SELF:_VFPMouseLeave?:Call()
				ENDIF
			ENDIF

		PRIVATE METHOD OnCellFormatting( sender AS OBJECT, e AS DataGridViewCellFormattingEventArgs ) AS VOID STRICT
			IF e:ColumnIndex != SELF:Index .OR. e:Value == NULL_OBJECT
				RETURN
			ENDIF
			VAR fmt := IIF( String.IsNullOrEmpty(SELF:_vfpFormat), "", SELF:_vfpFormat:ToUpper():Trim() )
			// @! — uppercase string value
			IF fmt:Contains("@!")
				e:Value           := e:Value:ToString():ToUpper()
				e:FormattingApplied := TRUE
			ENDIF
			// DynamicBackColor / DynamicForeColor — evaluate codeblock per cell
			IF SELF:_dynBackColorCB != NULL_OBJECT
				TRY
					e:CellStyle:BackColor := VFPTools.ColorFromVFP( (INT) Eval(SELF:_dynBackColorCB) )
                CATCH
                    NOP
				END TRY
			ENDIF
			IF SELF:_dynForeColorCB != NULL_OBJECT
				TRY
					e:CellStyle:ForeColor := VFPTools.ColorFromVFP( (INT) Eval(SELF:_dynForeColorCB) )
                CATCH
                    NOP
				END TRY
			ENDIF

		// VFP CurrentControl: name of the active editing control inside this column.
		// In WinForms the editing control is managed by DataGridView itself; this
		// property is stored so generated code can assign it without compile errors.
		PROPERTY CurrentControl AS STRING AUTO

		// VFP ColumnType: determines the cell editor type used in this column.
		// 0 = Default/Text, 3 = CheckBox, 5 = ComboBox (others not yet supported).
		// Swaps the CellTemplate so the DataGridView renders the correct cell type.
		PRIVATE _columnType AS INT
		PROPERTY ColumnType AS INT
			GET
				RETURN _columnType
			END GET
			SET
				_columnType := VALUE
				SWITCH VALUE
				CASE 3  // CheckBox
					SELF:CellTemplate := DataGridViewCheckBoxCell{}
					SELF:ValueType    := TypeOf(LOGIC)
				CASE 5  // ComboBox
					SELF:CellTemplate := DataGridViewComboBoxCell{}
					SELF:ValueType    := TypeOf(STRING)
				END SWITCH
			END SET
		END PROPERTY

		// VFP RowSourceType: how the ComboBox items are populated.
		// 0=None, 1=Value (comma-separated list), 2=Alias, 3=SQL, 5=Array.
		PRIVATE _rowSourceType AS INT
		PROPERTY RowSourceType AS INT
			GET
				RETURN _rowSourceType
			END GET
			SET
				_rowSourceType := VALUE
			END SET
		END PROPERTY

		// VFP RowSource: the data source for the ComboBox items.
		// For RowSourceType=1, a comma-separated list of values populated into the cell template.
		PRIVATE _rowSource AS STRING
		PROPERTY RowSource AS STRING
			GET
				RETURN _rowSource
			END GET
			SET
				_rowSource := VALUE
				IF _rowSourceType == 1 .AND. !String.IsNullOrEmpty(VALUE)
					IF SELF:CellTemplate IS DataGridViewComboBoxCell VAR comboCell
						comboCell:Items:Clear()
						FOREACH VAR item IN VALUE:Split( <CHAR>{ c',' } )
							comboCell:Items:Add( item:Trim() )
						NEXT
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		// VFP Enabled = .F. makes the column non-editable; maps to DataGridViewColumn.ReadOnly (inverse).
		PROPERTY Enabled AS LOGIC
			GET ; RETURN !SELF:ReadOnly ; END GET
			SET ; SELF:ReadOnly := !VALUE ; END SET
		END PROPERTY

		// VFP MousePointer — no per-column cursor in WinForms; stored for compatibility.
		PROPERTY MousePointer AS INT AUTO

		// VFP Sparse: .T. = only the active cell shows its editing control;
		// .F. = every cell in the column shows the control permanently.
		// Maps to DataGridViewColumn.Frozen is NOT the right mapping — WinForms
		// has no direct equivalent, so we store the value for completeness.
		PROPERTY Sparse AS LOGIC AUTO := TRUE

		// ── TextBox (configuration proxy) ────────────────────────────────────
		// VFP Column.TextBox is the embedded editing control.
		// WinForms equivalent is DataGridViewTextBoxEditingControl, accessible via
		// DataGridView.EditingControl at runtime while a cell is in edit mode.
		// This stub satisfies migrated code that reads Column.TextBox.InputMask etc.;
		// it is NOT the live editing control.
		PROPERTY TextBox AS TextBox AUTO

		// ── Resizable ─────────────────────────────────────────────────────────
		// VFP: .T.=user can resize column, .F.=fixed width
		// We shadow the base DataGridViewTriState property with a VFP-style LOGIC wrapper.
		NEW PROPERTY Resizable AS LOGIC
			GET ; RETURN SUPER:Resizable == DataGridViewTriState.True ; END GET
			SET
				SUPER:Resizable := IIF(VALUE, DataGridViewTriState.True, DataGridViewTriState.False)
			END SET
		END PROPERTY

// ── Tag ───────────────────────────────────────────────────────────────
// VFP Tag is a string; base DataGridViewBand.Tag is Object.
NEW PROPERTY Tag AS STRING
    GET
        LOCAL t := SUPER:Tag AS OBJECT
        RETURN IIF(t == NULL_OBJECT, "", t:ToString())
    END GET
    SET ; SUPER:Tag := (OBJECT) VALUE ; END SET
END PROPERTY

// ── SelectOnEntry ─────────────────────────────────────────────────────
// VFP: entering a column selects all cell content. No WinForms equivalent.
PROPERTY SelectOnEntry AS LOGIC AUTO

// ── FontOutline / FontShadow / DragMode ───────────────────────────────
// VFP-only; no WinForms equivalent. Stored for compatibility.
PROPERTY FontOutline AS LOGIC AUTO
PROPERTY FontShadow  AS LOGIC AUTO
PROPERTY DragMode    AS INT AUTO

// ── Refresh ───────────────────────────────────────────────────────────
METHOD Refresh() AS VOID STRICT
    IF SELF:DataGridView != NULL_OBJECT
        SELF:DataGridView:InvalidateColumn(SELF:Index)
    ENDIF

// ── Dispose: fire vfpDestroy ──────────────────────────────────────────
PROTECTED OVERRIDE METHOD Dispose(disposing AS LOGIC) AS VOID STRICT
    IF disposing
        SELF:_VFPDestroy?:Call()
    ENDIF
    SUPER:Dispose(disposing)

// ── vfp* event properties ─────────────────────────────────────────────
PRIVATE _VFPClick      AS VFPOverride
PRIVATE _VFPRightClick AS VFPOverride
PRIVATE _VFPDblClick   AS VFPOverride
PRIVATE _VFPGotFocus   AS VFPOverride
PRIVATE _VFPLostFocus  AS VFPOverride
PRIVATE _VFPMouseDown  AS VFPOverride
PRIVATE _VFPMouseUp    AS VFPOverride
PRIVATE _VFPMouseMove  AS VFPOverride
PRIVATE _VFPMouseEnter AS VFPOverride
PRIVATE _VFPMouseLeave AS VFPOverride
PRIVATE _VFPValid      AS VFPOverride
PRIVATE _VFPWhen       AS VFPOverride
PRIVATE _VFPKeyPress   AS VFPOverride
PRIVATE _VFPInit       AS VFPOverride
PRIVATE _VFPDestroy    AS VFPOverride
PRIVATE _VFPRefresh    AS VFPOverride
PRIVATE _initFired     AS LOGIC

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpClick AS STRING
    GET ; RETURN SELF:_VFPClick?:SendTo ; END GET
    SET ; SELF:_VFPClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpRightClick AS STRING
    GET ; RETURN SELF:_VFPRightClick?:SendTo ; END GET
    SET ; SELF:_VFPRightClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpDblClick AS STRING
    GET ; RETURN SELF:_VFPDblClick?:SendTo ; END GET
    SET ; SELF:_VFPDblClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpGotFocus AS STRING
    GET ; RETURN SELF:_VFPGotFocus?:SendTo ; END GET
    SET ; SELF:_VFPGotFocus := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpLostFocus AS STRING
    GET ; RETURN SELF:_VFPLostFocus?:SendTo ; END GET
    SET ; SELF:_VFPLostFocus := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseDown AS STRING
    GET ; RETURN SELF:_VFPMouseDown?:SendTo ; END GET
    SET ; SELF:_VFPMouseDown := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseUp AS STRING
    GET ; RETURN SELF:_VFPMouseUp?:SendTo ; END GET
    SET ; SELF:_VFPMouseUp := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseMove AS STRING
    GET ; RETURN SELF:_VFPMouseMove?:SendTo ; END GET
    SET ; SELF:_VFPMouseMove := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseEnter AS STRING
    GET ; RETURN SELF:_VFPMouseEnter?:SendTo ; END GET
    SET ; SELF:_VFPMouseEnter := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseLeave AS STRING
    GET ; RETURN SELF:_VFPMouseLeave?:SendTo ; END GET
    SET ; SELF:_VFPMouseLeave := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpValid AS STRING
    GET ; RETURN SELF:_VFPValid?:SendTo ; END GET
    SET ; SELF:_VFPValid := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpWhen AS STRING
    GET ; RETURN SELF:_VFPWhen?:SendTo ; END GET
    SET ; SELF:_VFPWhen := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpKeyPress AS STRING
    GET ; RETURN SELF:_VFPKeyPress?:SendTo ; END GET
    SET ; SELF:_VFPKeyPress := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpInit AS STRING
    GET ; RETURN SELF:_VFPInit?:SendTo ; END GET
    SET ; SELF:_VFPInit := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpDestroy AS STRING
    GET ; RETURN SELF:_VFPDestroy?:SendTo ; END GET
    SET ; SELF:_VFPDestroy := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpRefresh AS STRING
    GET ; RETURN SELF:_VFPRefresh?:SendTo ; END GET
    SET ; SELF:_VFPRefresh := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

// ── Cell event handlers ───────────────────────────────────────────────
PRIVATE METHOD OnCellMouseClick(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index .AND. e:RowIndex >= 0
        IF e:Button == System.Windows.Forms.MouseButtons.Right
            SELF:_VFPRightClick?:Call()
        ELSE
            SELF:_VFPClick?:Call()
        ENDIF
    ENDIF

PRIVATE METHOD OnCellDoubleClick(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index .AND. e:RowIndex >= 0
        SELF:_VFPDblClick?:Call()
    ENDIF

PRIVATE METHOD OnCellEnter(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index .AND. e:RowIndex >= 0
        SELF:_VFPGotFocus?:Call()
    ENDIF

PRIVATE METHOD OnCellLeave(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index .AND. e:RowIndex >= 0
        SELF:_VFPLostFocus?:Call()
    ENDIF

PRIVATE METHOD OnCellMouseDownInner(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index
        IF e:RowIndex == -1
            SELF:_GetHeader()?:FireMouseDown()
        ELSEIF e:RowIndex >= 0
            SELF:_VFPMouseDown?:Call()
        ENDIF
    ENDIF

PRIVATE METHOD OnCellMouseUpInner(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index
        IF e:RowIndex == -1
            SELF:_GetHeader()?:FireMouseUp()
        ELSEIF e:RowIndex >= 0
            SELF:_VFPMouseUp?:Call()
        ENDIF
    ENDIF

PRIVATE METHOD OnCellMouseMoveInner(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index
        IF e:RowIndex == -1
            SELF:_GetHeader()?:FireMouseMove()
        ELSEIF e:RowIndex >= 0
            SELF:_VFPMouseMove?:Call()
        ENDIF
    ENDIF

PRIVATE METHOD OnCellValidating(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellValidatingEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index
        SELF:_VFPValid?:Call()
    ENDIF

PRIVATE METHOD OnCellBeginEdit(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellCancelEventArgs) AS VOID STRICT
    IF e:ColumnIndex == SELF:Index
        SELF:_VFPWhen?:Call()
    ENDIF

// KeyPress on the DataGridView filters to the currently active cell's column.
PRIVATE METHOD OnGridKeyPress(sender AS OBJECT, e AS System.Windows.Forms.KeyPressEventArgs) AS VOID STRICT
    IF SELF:DataGridView:CurrentCell != NULL_OBJECT .AND. SELF:DataGridView:CurrentCell:ColumnIndex == SELF:Index
        SELF:_VFPKeyPress?:Call()
    ENDIF

// SelectOnEntry: when .T., select all cell text as soon as the editing control appears.
// BeginInvoke defers the SelectAll() until after the editing control is fully populated.
PRIVATE METHOD OnEditingControlShowing(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewEditingControlShowingEventArgs) AS VOID STRICT
    IF SELF:SelectOnEntry .AND. SELF:DataGridView:CurrentCell != NULL_OBJECT .AND. SELF:DataGridView:CurrentCell:ColumnIndex == SELF:Index
        IF e:Control IS System.Windows.Forms.TextBox VAR tb
            tb:SelectAll()
        ENDIF
    ENDIF

#include "FontProperties.xh"



	END CLASS
END NAMESPACE // XSharp.VFP.UI
