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
				// Wire ColumnHeaderMouseClick so Header.vfpClick fires
				SELF:DataGridView:ColumnHeaderMouseClick += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnColumnHeaderMouseClick() }
			ENDIF

		PRIVATE METHOD OnColumnHeaderMouseClick( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs ) AS VOID STRICT
			IF e:ColumnIndex == SELF:Index
				IF SELF:HeaderCell IS Header VAR hdr
					hdr:FireClick()
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

		// VFP CurrentControl: name of the active editing control inside this column.
		// In WinForms the editing control is managed by DataGridView itself; this
		// property is stored so generated code can assign it without compile errors.
		PROPERTY CurrentControl AS STRING AUTO

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

#include "FontProperties.xh"



	END CLASS
END NAMESPACE // XSharp.VFP.UI
