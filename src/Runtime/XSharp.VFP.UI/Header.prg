// Header.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel
USING System.Drawing
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible column header control that wraps <see cref="System.Windows.Forms.DataGridViewColumnHeaderCell"/>.<br/>
	/// Exposes the standard VFP Header properties (<see cref="Caption"/>, <see cref="ForeColor"/>,
	/// <see cref="BackColor"/>, font attributes, <see cref="Alignment"/>, <see cref="Picture"/>)
	/// and the VFP mouse/click events (<see cref="vfpClick"/>, <see cref="vfpRightClick"/>,
	/// <see cref="vfpDblClick"/>, <see cref="vfpMouseDown"/>, <see cref="vfpMouseUp"/>,
	/// <see cref="vfpMouseMove"/>, <see cref="vfpMouseEnter"/>, <see cref="vfpMouseLeave"/>).<br/>
	/// Because <c>DataGridViewColumnHeaderCell</c> is not a <c>Control</c>, mouse events are wired
	/// indirectly by the owning <see cref="Column"/> inside its <c>OnDataGridViewChanged</c> override.
	/// Font and colour changes write to <see cref="System.Windows.Forms.DataGridViewCellStyle"/> on
	/// <c>Style</c>; call <see cref="Refresh"/> to force an immediate repaint.
	/// </summary>
	CLASS Header INHERIT System.Windows.Forms.DataGridViewColumnHeaderCell

		CONSTRUCTOR(  )
			SUPER()
			RETURN

		CONSTRUCTOR(  source AS System.Windows.Forms.DataGridViewColumnHeaderCell )
			SUPER()
			//
			SELF:ErrorText := source:ErrorText
            SELF:Tag := source:Tag
            SELF:ToolTipText := source:ToolTipText
            SELF:Value := source:Value
            SELF:ContextMenuStrip := source:ContextMenuStrip
            SELF:ValueType := source:ValueType
            IF (source:HasStyle)
                SELF:Style := source:Style
			ENDIF
			RETURN

		/// <summary>
		/// The label displayed in the column header cell — maps to the underlying <c>DataGridViewColumnHeaderCell.Value</c>.<br/>
		/// Setting <see cref="Caption"/> writes to this property; reading either returns the same string.
		/// </summary>
		PROPERTY Text AS STRING
			GET
				IF SELF:Value != NULL
					RETURN SELF:Value:ToString()
				ELSE
					RETURN NULL
				ENDIF
			END GET
			SET
				SUPER:Value := (OBJECT) VALUE
			END SET
		END PROPERTY

		// ── Caption ───────────────────────────────────────────────────────────
		/// <summary>VFP Caption — the header label text. Alias for <see cref="Text"/>; both properties read and write the same underlying value.</summary>
		PROPERTY Caption AS STRING
			GET ; RETURN SELF:Text ; END GET
			SET ; SELF:Text := VALUE ; END SET
		END PROPERTY

		/// <summary>Programmatic name of the Header object — used by VFP code to reference the header.</summary>
		PROPERTY Name AS STRING AUTO

		// ── FontSize ─────────────────────────────────────────────────────────
		/// <summary>
		/// Font size (in points) applied to the column header text via <c>Style.Font</c>.<br/>
		/// Returns 0 when no explicit font has been set (the grid's default style applies).
		/// Setting a value ≤ 0 is ignored.
		/// </summary>
		PROPERTY FontSize AS LONG
			GET
				IF SELF:Style:Font != NULL
					RETURN (LONG) SELF:Style:Font:Size
				ENDIF
				RETURN 0
			END GET
			SET
				IF VALUE > 0
					VAR existing := IIF(SELF:Style:Font != NULL, SELF:Style:Font, SystemFonts.DefaultFont)
					SELF:Style:Font := Font{existing:FontFamily, (SINGLE) VALUE, existing:Style}
				ENDIF
			END SET
		END PROPERTY

		// ── ForeColor / BackColor ─────────────────────────────────────────────
		/// <summary>Foreground (text) colour of the column header, written to <c>Style.ForeColor</c>.</summary>
		PROPERTY ForeColor AS System.Drawing.Color
			GET ; RETURN SELF:Style:ForeColor ; END GET
			SET ; SELF:Style:ForeColor := VALUE ; END SET
		END PROPERTY

		/// <summary>Background colour of the column header cell, written to <c>Style.BackColor</c>.</summary>
		PROPERTY BackColor AS System.Drawing.Color
			GET ; RETURN SELF:Style:BackColor ; END GET
			SET ; SELF:Style:BackColor := VALUE ; END SET
		END PROPERTY

		// ── Font helpers ──────────────────────────────────────────────────────
		PRIVATE METHOD _GetStyle() AS System.Drawing.Font
			RETURN IIF(SELF:Style:Font != NULL, SELF:Style:Font, SystemFonts.DefaultFont)
		END METHOD

		/// <summary>Font family name of the column header text, written to <c>Style.Font</c>. Falls back to the system default font when no style font has been set.</summary>
		PROPERTY FontName AS STRING
			GET ; RETURN SELF:_GetStyle():Name ; END GET
			SET
				VAR f := SELF:_GetStyle()
				SELF:Style:Font := System.Drawing.Font{VALUE, f:Size, f:Style}
			END SET
		END PROPERTY

		/// <summary>When <c>.T.</c>, renders the header text in italic. Modifies <c>Style.Font</c> while preserving the existing family, size, and other style bits.</summary>
		PROPERTY FontItalic AS LOGIC
			GET ; RETURN SELF:_GetStyle():Italic ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Italic, f:Style & ~System.Drawing.FontStyle.Italic)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		/// <summary>When <c>.T.</c>, underlines the header text. Modifies <c>Style.Font</c> while preserving the existing family, size, and other style bits.</summary>
		PROPERTY FontUnderline AS LOGIC
			GET ; RETURN SELF:_GetStyle():Underline ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Underline, f:Style & ~System.Drawing.FontStyle.Underline)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		// ── FontBold ──────────────────────────────────────────────────────────
		/// <summary>When <c>.T.</c>, renders the header text bold. Modifies <c>Style.Font</c> while preserving the existing family, size, and other style bits.</summary>
		PROPERTY FontBold AS LOGIC
			GET
				IF SELF:Style:Font != NULL
					RETURN SELF:Style:Font:Bold
				ENDIF
				RETURN FALSE
			END GET
			SET
				VAR existing := IIF(SELF:Style:Font != NULL, SELF:Style:Font, SystemFonts.DefaultFont)
				VAR style    := IIF(VALUE, existing:Style | FontStyle.Bold, existing:Style & ~FontStyle.Bold)
				SELF:Style:Font := Font{existing:FontFamily, existing:Size, style}
			END SET
		END PROPERTY

		// ── Alignment ─────────────────────────────────────────────────────────
		/// <summary>
		/// Horizontal alignment of the header text: 0=Left (default), 1=Right, 2=Center.<br/>
		/// Maps to <see cref="System.Windows.Forms.DataGridViewCellStyle.Alignment"/> using the
		/// <c>MiddleLeft</c> / <c>MiddleRight</c> / <c>MiddleCenter</c> values so the text is always
		/// vertically centred.
		/// </summary>
		PROPERTY Alignment AS LONG
			GET
				SWITCH SELF:Style:Alignment
				CASE DataGridViewContentAlignment.MiddleRight  ; RETURN 1
				CASE DataGridViewContentAlignment.MiddleCenter ; RETURN 2
				OTHERWISE                                      ; RETURN 0
				END SWITCH
			END GET
			SET
				SWITCH VALUE
				CASE 1 ; SELF:Style:Alignment := DataGridViewContentAlignment.MiddleRight
				CASE 2 ; SELF:Style:Alignment := DataGridViewContentAlignment.MiddleCenter
				OTHERWISE ; SELF:Style:Alignment := DataGridViewContentAlignment.MiddleLeft
				END SWITCH
			END SET
		END PROPERTY

		// ── FontStrikeThru ───────────────────────────────────────────────────
		/// <summary>When <c>.T.</c>, draws a strikethrough line through the header text. Modifies <c>Style.Font</c> while preserving the existing family, size, and other style bits.</summary>
		PROPERTY FontStrikeThru AS LOGIC
			GET ; RETURN SELF:_GetStyle():Strikeout ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Strikeout, f:Style & ~System.Drawing.FontStyle.Strikeout)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		// ── Picture ───────────────────────────────────────────────────────────
		/// <summary>
		/// File path to an image displayed in the column header.<br/>
		/// <c>DataGridViewColumnHeaderCell</c> has no built-in <c>Image</c> property; the image is
		/// loaded via <see cref="VFPTools.ImageFromFile"/> and the owning <see cref="System.Windows.Forms.DataGridView"/>
		/// is invalidated so the column repaints. Set to an empty string or <c>NULL</c> to remove the image.
		/// </summary>
		PRIVATE _picture      AS STRING
		PRIVATE _headerImage  AS System.Drawing.Image
		PROPERTY Picture AS STRING
			GET ; RETURN IIF(SELF:_picture == NULL, "", SELF:_picture) ; END GET
			SET
				SELF:_picture     := VALUE
				SELF:_headerImage := IIF(String.IsNullOrEmpty(VALUE), NULL_OBJECT, VFPTools.ImageFromFile(VALUE))
				SELF:DataGridView?:InvalidateColumn(SELF:ColumnIndex)
			END SET
		END PROPERTY

		// ── Click event ───────────────────────────────────────────────────────
		// VFP Header.Click fires when the user clicks the column header.
		// Since DataGridViewColumnHeaderCell is not a Control, we wire into
		// DataGridView.ColumnHeaderMouseClick from within the owning Column.
		// This property stores the handler reference; Column.OnDataGridViewChanged wires it.
		PRIVATE _VFPClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the user clicks the column header. Wired via <see cref="Column"/>'s <c>OnDataGridViewChanged</c> to <c>DataGridView.ColumnHeaderMouseClick</c>.</summary>
		PROPERTY vfpClick AS STRING
			GET ; RETURN SELF:_VFPClick?:SendTo ; END GET
			SET ; SELF:_VFPClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpClick"/> handler. Called by the owning <see cref="Column"/> when a left-button header click is detected.</summary>
		METHOD FireClick() AS VOID STRICT
			IF SELF:_VFPClick != NULL
				SELF:_VFPClick:Call()
			ENDIF

		// ── RightClick event ─────────────────────────────────────────────────
		PRIVATE _VFPRightClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the user right-clicks the column header.</summary>
		PROPERTY vfpRightClick AS STRING
			GET ; RETURN SELF:_VFPRightClick?:SendTo ; END GET
			SET ; SELF:_VFPRightClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpRightClick"/> handler. Called by the owning <see cref="Column"/> on a right-button header click.</summary>
		METHOD FireRightClick() AS VOID STRICT
			IF SELF:_VFPRightClick != NULL
				SELF:_VFPRightClick:Call()
			ENDIF

		// ── DblClick event ────────────────────────────────────────────────────
		PRIVATE _VFPDblClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the user double-clicks the column header.</summary>
		PROPERTY vfpDblClick AS STRING
			GET ; RETURN SELF:_VFPDblClick?:SendTo ; END GET
			SET ; SELF:_VFPDblClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpDblClick"/> handler. Called by the owning <see cref="Column"/> on a header double-click.</summary>
		METHOD FireDblClick() AS VOID STRICT
			IF SELF:_VFPDblClick != NULL
				SELF:_VFPDblClick:Call()
			ENDIF

		// ── MouseDown event ───────────────────────────────────────────────────
		PRIVATE _VFPMouseDown AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when a mouse button is pressed over the column header.</summary>
		PROPERTY vfpMouseDown AS STRING
			GET ; RETURN SELF:_VFPMouseDown?:SendTo ; END GET
			SET ; SELF:_VFPMouseDown := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpMouseDown"/> handler.</summary>
		METHOD FireMouseDown() AS VOID STRICT
			IF SELF:_VFPMouseDown != NULL
				SELF:_VFPMouseDown:Call()
			ENDIF

		// ── MouseUp event ─────────────────────────────────────────────────────
		PRIVATE _VFPMouseUp AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when a mouse button is released over the column header.</summary>
		PROPERTY vfpMouseUp AS STRING
			GET ; RETURN SELF:_VFPMouseUp?:SendTo ; END GET
			SET ; SELF:_VFPMouseUp := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpMouseUp"/> handler.</summary>
		METHOD FireMouseUp() AS VOID STRICT
			IF SELF:_VFPMouseUp != NULL
				SELF:_VFPMouseUp:Call()
			ENDIF

		// ── MouseMove event ───────────────────────────────────────────────────
		PRIVATE _VFPMouseMove AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the mouse pointer moves over the column header.</summary>
		PROPERTY vfpMouseMove AS STRING
			GET ; RETURN SELF:_VFPMouseMove?:SendTo ; END GET
			SET ; SELF:_VFPMouseMove := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpMouseMove"/> handler.</summary>
		METHOD FireMouseMove() AS VOID STRICT
			IF SELF:_VFPMouseMove != NULL
				SELF:_VFPMouseMove:Call()
			ENDIF

		// ── MouseEnter event ──────────────────────────────────────────────────
		PRIVATE _VFPMouseEnter AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the mouse pointer enters the column header area.</summary>
		PROPERTY vfpMouseEnter AS STRING
			GET ; RETURN SELF:_VFPMouseEnter?:SendTo ; END GET
			SET ; SELF:_VFPMouseEnter := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpMouseEnter"/> handler.</summary>
		METHOD FireMouseEnter() AS VOID STRICT
			IF SELF:_VFPMouseEnter != NULL
				SELF:_VFPMouseEnter:Call()
			ENDIF

		// ── MouseLeave event ──────────────────────────────────────────────────
		PRIVATE _VFPMouseLeave AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method to call when the mouse pointer leaves the column header area.</summary>
		PROPERTY vfpMouseLeave AS STRING
			GET ; RETURN SELF:_VFPMouseLeave?:SendTo ; END GET
			SET ; SELF:_VFPMouseLeave := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		/// <summary>Invokes the <see cref="vfpMouseLeave"/> handler.</summary>
		METHOD FireMouseLeave() AS VOID STRICT
			IF SELF:_VFPMouseLeave != NULL
				SELF:_VFPMouseLeave:Call()
			ENDIF

		// ── Refresh stub ──────────────────────────────────────────────────────
		/// <summary>
		/// Redraws the column header.<br/>
		/// <c>DataGridViewColumnHeaderCell</c> has no direct invalidate API, so this method calls
		/// <see cref="System.Windows.Forms.DataGridView.Invalidate()"/> on the owning grid when available.
		/// Has no effect if the header is not yet attached to a grid.
		/// </summary>
		METHOD Refresh() AS VOID STRICT
			IF SELF:DataGridView != NULL_OBJECT
				SELF:DataGridView:Invalidate()
			ENDIF

		PROPERTY TextAlign AS System.Drawing.ContentAlignment AUTO

		OVERRIDE METHOD Clone() AS OBJECT
			LOCAL source AS Header
			//
			source := Header{}
			SELF:ErrorText := source:ErrorText
            SELF:Tag := source:Tag
            SELF:ToolTipText := source:ToolTipText
            SELF:Value := source:Value
            SELF:ContextMenuStrip := source:ContextMenuStrip
            SELF:ValueType := source:ValueType
            IF (source:HasStyle)
                SELF:Style := source:Style
			ENDIF
			source:Name := SELF:Name
			//
			RETURN source

	END CLASS
END NAMESPACE // XSharp.VFP.UI
