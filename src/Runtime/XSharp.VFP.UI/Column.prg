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
	/// VFP-compatible grid column that wraps <see cref="System.Windows.Forms.DataGridViewTextBoxColumn"/>.<br/>
	/// Exposes the full set of VFP Column properties: <see cref="ControlSource"/> (→ DataPropertyName),
	/// <see cref="Format"/> / <see cref="InputMask"/> (mapped to <c>DefaultCellStyle.Format</c> plus a
	/// CellFormatting handler for codes that have no .NET equivalent), <see cref="ColumnType"/>
	/// (0=text, 3=checkbox, 5=combobox), <see cref="RowSourceType"/> / <see cref="RowSource"/>,
	/// <see cref="DynamicBackColor"/> / <see cref="DynamicForeColor"/> (VFP expression strings evaluated
	/// per cell), <see cref="Alignment"/>, <see cref="ColumnOrder"/> (deferred <c>DisplayIndex</c>),
	/// <see cref="Header"/>, <see cref="Enabled"/>, <see cref="Sparse"/>, <see cref="SelectOnEntry"/>,
	/// <see cref="Resizable"/>, and <see cref="Tag"/>.<br/>
	/// VFP events (<see cref="vfpClick"/>, <see cref="vfpRightClick"/>, <see cref="vfpDblClick"/>,
	/// <see cref="vfpGotFocus"/>, <see cref="vfpLostFocus"/>, <see cref="vfpMouseDown"/>,
	/// <see cref="vfpMouseUp"/>, <see cref="vfpMouseMove"/>, <see cref="vfpMouseEnter"/>,
	/// <see cref="vfpMouseLeave"/>, <see cref="vfpValid"/>, <see cref="vfpWhen"/>,
	/// <see cref="vfpKeyPress"/>, <see cref="vfpInit"/>, <see cref="vfpDestroy"/>,
	/// <see cref="vfpRefresh"/>) are all wired in <see cref="OnDataGridViewChanged"/> once the column
	/// is attached to a <see cref="System.Windows.Forms.DataGridView"/>.
	/// </summary>
	PARTIAL CLASS Column INHERIT System.Windows.Forms.DataGridViewTextBoxColumn IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		CONSTRUCTOR(  )
            SUPER()
            SELF:Width := 75
			RETURN

		/// <summary>
		/// VFP ControlSource — the cursor field that this column displays and edits.<br/>
		/// Maps to <see cref="System.Windows.Forms.DataGridViewColumn.DataPropertyName"/>. If the value
		/// contains a dot (alias prefix such as <c>"customer.name"</c>), only the field part after the
		/// dot is used so that WinForms data binding resolves correctly against the bound
		/// <see cref="System.Data.DataTable"/> column name.
		/// </summary>
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

		/// <summary>
		/// Font applied to all data cells in the column via <c>DefaultCellStyle.Font</c>.
		/// </summary>
		PROPERTY Font AS System.Drawing.Font GET SELF:DefaultCellStyle:Font SET SELF:DefaultCellStyle:Font := VALUE

		// ── BackColor / ForeColor ─────────────────────────────────────────────
		// Map to DefaultCellStyle so the colour applies to all cells in this column.

		/// <summary>
		/// Background colour for all data cells in this column, written to <c>DefaultCellStyle.BackColor</c>.
		/// </summary>
		PROPERTY BackColor AS System.Drawing.Color
			GET
				RETURN SELF:DefaultCellStyle:BackColor
			END GET
			SET
				SELF:DefaultCellStyle:BackColor := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Text (foreground) colour for all data cells in this column, written to <c>DefaultCellStyle.ForeColor</c>.
		/// </summary>
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

		/// <summary>
		/// VFP DynamicBackColor expression — a VFP colour expression (e.g. <c>"RGB(255,0,0)"</c>)
		/// evaluated per cell during <c>CellFormatting</c>.<br/>
		/// The string is compiled to a codeblock on assignment; the result is converted to a
		/// <see cref="System.Drawing.Color"/> via <see cref="VFPTools.ColorFromVFP"/>.
		/// Set to an empty string to disable dynamic colouring.
		/// </summary>
		PROPERTY DynamicBackColor AS STRING
			GET
				RETURN SELF:_dynBackColorStr
			END GET
			SET
				SELF:_dynBackColorStr := VALUE
				SELF:_dynBackColorCB  := IIF( String.IsNullOrEmpty(VALUE), NULL_OBJECT, MCompile("{|| " + VALUE + "}") )
			END SET
		END PROPERTY

		/// <summary>
		/// VFP DynamicForeColor expression — a VFP colour expression evaluated per cell during <c>CellFormatting</c>.<br/>
		/// Works identically to <see cref="DynamicBackColor"/> but targets the cell text colour.
		/// </summary>
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

		/// <summary>
		/// Horizontal alignment of cell content: 0=Left (default), 1=Right, 2=Center.<br/>
		/// Maps to <see cref="System.Windows.Forms.DataGridViewCellStyle.Alignment"/> on
		/// <c>DefaultCellStyle</c> using <c>MiddleLeft</c> / <c>MiddleRight</c> / <c>MiddleCenter</c>
		/// so content is always vertically centred.
		/// </summary>
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
		/// <summary>
		/// VFP 1-based display order of the column within the grid.<br/>
		/// Maps to <see cref="System.Windows.Forms.DataGridViewColumn.DisplayIndex"/> (0-based).
		/// Because <c>DisplayIndex</c> can only be set after the column is added to a
		/// <see cref="System.Windows.Forms.DataGridView"/>, the value is stored and applied lazily
		/// inside <see cref="OnDataGridViewChanged"/> when the grid becomes available.
		/// </summary>
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

		/// <summary>
		/// The VFP <see cref="Header"/> object for this column — provides <c>Caption</c>, font,
		/// colour, and event properties for the column header cell.<br/>
		/// On first access, if the underlying <c>HeaderCell</c> is not already a <see cref="Header"/>,
		/// it is promoted to one (copying existing cell properties) and stored back.
		/// </summary>
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
		/// <summary>
		/// VFP Format string for the column (e.g. <c>"@!"</c>, <c>"@D"</c>, <c>"@DT"</c>, <c>"$"</c>).<br/>
		/// Where a direct <c>DataGridViewCellStyle.Format</c> mapping exists it is applied immediately
		/// via <c>ApplyVFPFormat</c>: <c>@D</c>/<c>@DL</c>/<c>@DS</c>→<c>"d"</c>,
		/// <c>@DT</c>→<c>"g"</c>, <c>$</c>→<c>"C2"</c>, numeric masks→<c>"N{n}"</c>.<br/>
		/// Codes with no .NET equivalent (such as <c>@!</c> for uppercase) are applied per-cell in the
		/// <c>CellFormatting</c> handler wired in <see cref="OnDataGridViewChanged"/>.
		/// </summary>
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

		/// <summary>
		/// VFP positional InputMask for the column (e.g. <c>"9,999.99"</c>, <c>"$9,999.99"</c>).<br/>
		/// Processed together with <see cref="Format"/> by <c>ApplyVFPFormat</c>: the decimal-place
		/// count drives <c>"N{n}"</c> formatting, and a <c>$</c> prefix drives <c>"C2"</c>.
		/// </summary>
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

		/// <summary>
		/// Called when the column is added to or removed from a <see cref="System.Windows.Forms.DataGridView"/>.<br/>
		/// When a grid becomes available: applies any pending <see cref="ColumnOrder"/>,
		/// wires all VFP event delegates (<c>CellFormatting</c>, header mouse events,
		/// cell click/enter/leave/validate/edit events, key-press, editing-control-showing),
		/// and fires <see cref="vfpInit"/> the first time the column is attached.
		/// </summary>
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

		/// <summary>
		/// VFP CurrentControl — name of the active embedded editing control. Stored for compatibility; WinForms manages the editing control internally via <c>DataGridView.EditingControl</c>.
		/// </summary>
		PROPERTY CurrentControl AS STRING AUTO

		/// <summary>
		/// VFP ColumnType — determines the cell editor rendered in this column.<br/>
		/// 0 (default) = text box; 3 = check box (<see cref="System.Windows.Forms.DataGridViewCheckBoxCell"/>);
		/// 5 = combo box (<see cref="System.Windows.Forms.DataGridViewComboBoxCell"/>).<br/>
		/// Changing this value replaces <c>CellTemplate</c> so the <see cref="System.Windows.Forms.DataGridView"/>
		/// immediately uses the new cell type for all rows.
		/// </summary>
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

		/// <summary>
		/// VFP RowSourceType — controls how the combo-box column's list is populated.<br/>
		/// 0=None, 1=Value (comma-separated <see cref="RowSource"/> string), 2=Alias,
		/// 3=SQL statement, 5=Array. Only type 1 is fully implemented at runtime;
		/// other types are stored for compatibility.
		/// </summary>
		PRIVATE _rowSourceType AS INT
		PROPERTY RowSourceType AS INT
			GET
				RETURN _rowSourceType
			END GET
			SET
				_rowSourceType := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// VFP RowSource — data source for the combo-box column.<br/>
		/// When <see cref="RowSourceType"/> is 1, this is a comma-separated list of values
		/// that is split and loaded directly into the <see cref="System.Windows.Forms.DataGridViewComboBoxCell"/>
		/// template's <c>Items</c> collection.
		/// </summary>
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

		/// <summary>
		/// When <c>.F.</c>, prevents the user from editing cells in this column. Inverse of <see cref="System.Windows.Forms.DataGridViewColumn.ReadOnly"/>.
		/// </summary>
		PROPERTY Enabled AS LOGIC
			GET ; RETURN !SELF:ReadOnly ; END GET
			SET ; SELF:ReadOnly := !VALUE ; END SET
		END PROPERTY

		/// <summary>
		/// VFP MousePointer — no per-column cursor API exists in WinForms; stored for source compatibility.
		/// </summary>
		PROPERTY MousePointer AS INT AUTO

		/// <summary>
		/// VFP Sparse — when <c>.T.</c> (default), only the active cell shows its editing control;
		/// when <c>.F.</c>, all cells permanently show the control.<br/>
		/// WinForms has no direct equivalent; this value is stored for compatibility only.
		/// </summary>
		PROPERTY Sparse AS LOGIC AUTO := TRUE

		/// <summary>
		/// Proxy for the VFP embedded TextBox editing control within the column.<br/>
		/// The live editing control is <c>DataGridView.EditingControl</c> and is only available
		/// while a cell is in edit mode. This stub allows migrated VFP code that reads
		/// <c>Column.TextBox.InputMask</c> etc. to compile without errors.
		/// </summary>
		PROPERTY TextBox AS TextBox AUTO

		/// <summary>
		/// When <c>.T.</c>, the user can resize the column by dragging its header border.<br/>
		/// Wraps the base <c>DataGridViewTriState</c> <c>Resizable</c> property as a plain VFP LOGIC value.
		/// </summary>
		NEW PROPERTY Resizable AS LOGIC
			GET ; RETURN SUPER:Resizable == DataGridViewTriState.True ; END GET
			SET
				SUPER:Resizable := IIF(VALUE, DataGridViewTriState.True, DataGridViewTriState.False)
			END SET
		END PROPERTY

// ── Tag ───────────────────────────────────────────────────────────────
/// <summary>
/// VFP Tag — arbitrary string value attached to the column. Shadows the base <c>Object</c>-typed <c>Tag</c> with a <c>STRING</c>-typed wrapper.
/// </summary>
NEW PROPERTY Tag AS STRING
    GET
        LOCAL t := SUPER:Tag AS OBJECT
        RETURN IIF(t == NULL_OBJECT, "", t:ToString())
    END GET
    SET ; SUPER:Tag := (OBJECT) VALUE ; END SET
END PROPERTY

/// <summary>
/// When <c>.T.</c>, selecting a cell automatically selects all its text when the editing control appears. Implemented via the <c>EditingControlShowing</c> event.
/// </summary>
PROPERTY SelectOnEntry AS LOGIC AUTO

// ── FontOutline / FontShadow / DragMode ───────────────────────────────
/// <summary>
/// VFP FontOutline — no WinForms equivalent; stored for source compatibility.
/// </summary>
PROPERTY FontOutline AS LOGIC AUTO
/// <summary>
/// VFP FontShadow — no WinForms equivalent; stored for source compatibility.
/// </summary>
PROPERTY FontShadow  AS LOGIC AUTO
/// <summary>
/// VFP DragMode — no WinForms equivalent; stored for source compatibility.
/// </summary>
PROPERTY DragMode    AS INT AUTO

// ── Refresh ───────────────────────────────────────────────────────────
/// <summary>
/// Repaints this column by calling <see cref="System.Windows.Forms.DataGridView.InvalidateColumn"/>. Has no effect when the column is not attached to a grid.
/// </summary>
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
// All vfp* properties hold the name of the VFP method to call when the corresponding
// event fires. Each is compiled into a VFPOverride codeblock on assignment.
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

/// <summary>
/// Name of the VFP method called when the user left-clicks a data cell in this column.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpClick AS STRING
    GET ; RETURN SELF:_VFPClick?:SendTo ; END GET
    SET ; SELF:_VFPClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the user right-clicks a data cell in this column.
///</summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpRightClick AS STRING
    GET ; RETURN SELF:_VFPRightClick?:SendTo ; END GET
    SET ; SELF:_VFPRightClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the user double-clicks a data cell in this column.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpDblClick AS STRING
    GET ; RETURN SELF:_VFPDblClick?:SendTo ; END GET
    SET ; SELF:_VFPDblClick := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when a cell in this column receives focus (mapped to <c>DataGridView.CellEnter</c>).
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpGotFocus AS STRING
    GET ; RETURN SELF:_VFPGotFocus?:SendTo ; END GET
    SET ; SELF:_VFPGotFocus := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when a cell in this column loses focus (mapped to <c>DataGridView.CellLeave</c>).
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpLostFocus AS STRING
    GET ; RETURN SELF:_VFPLostFocus?:SendTo ; END GET
    SET ; SELF:_VFPLostFocus := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when a mouse button is pressed over a data cell or the column header.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseDown AS STRING
    GET ; RETURN SELF:_VFPMouseDown?:SendTo ; END GET
    SET ; SELF:_VFPMouseDown := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when a mouse button is released over a data cell or the column header.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseUp AS STRING
    GET ; RETURN SELF:_VFPMouseUp?:SendTo ; END GET
    SET ; SELF:_VFPMouseUp := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the mouse pointer moves over a data cell in this column.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseMove AS STRING
    GET ; RETURN SELF:_VFPMouseMove?:SendTo ; END GET
    SET ; SELF:_VFPMouseMove := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the mouse pointer enters a data cell in this column.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseEnter AS STRING
    GET ; RETURN SELF:_VFPMouseEnter?:SendTo ; END GET
    SET ; SELF:_VFPMouseEnter := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the mouse pointer leaves a data cell in this column.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpMouseLeave AS STRING
    GET ; RETURN SELF:_VFPMouseLeave?:SendTo ; END GET
    SET ; SELF:_VFPMouseLeave := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called during cell validation (mapped to <c>DataGridView.CellValidating</c>). Return <c>.F.</c> to cancel the move.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpValid AS STRING
    GET ; RETURN SELF:_VFPValid?:SendTo ; END GET
    SET ; SELF:_VFPValid := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called before a cell enters edit mode (mapped to <c>DataGridView.CellBeginEdit</c>). Return <c>.F.</c> to prevent editing.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpWhen AS STRING
    GET ; RETURN SELF:_VFPWhen?:SendTo ; END GET
    SET ; SELF:_VFPWhen := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when a key is pressed while a cell in this column is active (mapped to <c>DataGridView.KeyPress</c>).
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpKeyPress AS STRING
    GET ; RETURN SELF:_VFPKeyPress?:SendTo ; END GET
    SET ; SELF:_VFPKeyPress := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called once when the column is first attached to a <see cref="System.Windows.Forms.DataGridView"/>.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpInit AS STRING
    GET ; RETURN SELF:_VFPInit?:SendTo ; END GET
    SET ; SELF:_VFPInit := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called when the column is disposed.
/// </summary>
[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
PROPERTY vfpDestroy AS STRING
    GET ; RETURN SELF:_VFPDestroy?:SendTo ; END GET
    SET ; SELF:_VFPDestroy := VFPOverride{NULL, VALUE} ; END SET
END PROPERTY

/// <summary>
/// Name of the VFP method called to refresh the column's visual state.
/// </summary>
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
