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
	/// The VFP compatible Header class.
	/// Maps to DataGridViewColumnHeaderCell.
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
		/// Text is the Value of the HeaderCell
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
		// VFP Caption is the header label; alias for Text.
		PROPERTY Caption AS STRING
			GET ; RETURN SELF:Text ; END GET
			SET ; SELF:Text := VALUE ; END SET
		END PROPERTY

		PROPERTY Name AS STRING AUTO

		// ── FontSize ─────────────────────────────────────────────────────────
		// Applies to the header cell's style font.
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
		PROPERTY ForeColor AS System.Drawing.Color
			GET ; RETURN SELF:Style:ForeColor ; END GET
			SET ; SELF:Style:ForeColor := VALUE ; END SET
		END PROPERTY

		PROPERTY BackColor AS System.Drawing.Color
			GET ; RETURN SELF:Style:BackColor ; END GET
			SET ; SELF:Style:BackColor := VALUE ; END SET
		END PROPERTY

		// ── Font helpers ──────────────────────────────────────────────────────
		PRIVATE METHOD _GetStyle() AS System.Drawing.Font
			RETURN IIF(SELF:Style:Font != NULL, SELF:Style:Font, SystemFonts.DefaultFont)
		END METHOD

		PROPERTY FontName AS STRING
			GET ; RETURN SELF:_GetStyle():Name ; END GET
			SET
				VAR f := SELF:_GetStyle()
				SELF:Style:Font := System.Drawing.Font{VALUE, f:Size, f:Style}
			END SET
		END PROPERTY

		PROPERTY FontItalic AS LOGIC
			GET ; RETURN SELF:_GetStyle():Italic ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Italic, f:Style & ~System.Drawing.FontStyle.Italic)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		PROPERTY FontUnderline AS LOGIC
			GET ; RETURN SELF:_GetStyle():Underline ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Underline, f:Style & ~System.Drawing.FontStyle.Underline)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		// ── FontBold ──────────────────────────────────────────────────────────
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
		// VFP: 0=Left, 1=Right, 2=Center
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
		PROPERTY FontStrikeThru AS LOGIC
			GET ; RETURN SELF:_GetStyle():Strikeout ; END GET
			SET
				VAR f := SELF:_GetStyle()
				VAR s := IIF(VALUE, f:Style | System.Drawing.FontStyle.Strikeout, f:Style & ~System.Drawing.FontStyle.Strikeout)
				SELF:Style:Font := System.Drawing.Font{f:FontFamily, f:Size, s}
			END SET
		END PROPERTY

		// ── Picture ───────────────────────────────────────────────────────────
		// VFP Picture: file path to an image shown in the column header.
		// Maps to DataGridViewColumnHeaderCell.Image.
		PRIVATE _picture AS STRING
		PROPERTY Picture AS STRING
			GET ; RETURN IIF(SELF:_picture == NULL, "", SELF:_picture) ; END GET
			SET
				SELF:_picture := VALUE
				SELF:Image := IIF(String.IsNullOrEmpty(VALUE), NULL_OBJECT, VFPTools.ImageFromFile(VALUE))
			END SET
		END PROPERTY

		// ── Click event ───────────────────────────────────────────────────────
		// VFP Header.Click fires when the user clicks the column header.
		// Since DataGridViewColumnHeaderCell is not a Control, we wire into
		// DataGridView.ColumnHeaderMouseClick from within the owning Column.
		// This property stores the handler reference; Column.OnDataGridViewChanged wires it.
		PRIVATE _VFPClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpClick AS STRING
			GET ; RETURN SELF:_VFPClick?:SendTo ; END GET
			SET ; SELF:_VFPClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireClick() AS VOID STRICT
			IF SELF:_VFPClick != NULL
				SELF:_VFPClick:Call()
			ENDIF

		// ── RightClick event ─────────────────────────────────────────────────
		PRIVATE _VFPRightClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpRightClick AS STRING
			GET ; RETURN SELF:_VFPRightClick?:SendTo ; END GET
			SET ; SELF:_VFPRightClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireRightClick() AS VOID STRICT
			IF SELF:_VFPRightClick != NULL
				SELF:_VFPRightClick:Call()
			ENDIF

		// ── DblClick event ────────────────────────────────────────────────────
		PRIVATE _VFPDblClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpDblClick AS STRING
			GET ; RETURN SELF:_VFPDblClick?:SendTo ; END GET
			SET ; SELF:_VFPDblClick := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireDblClick() AS VOID STRICT
			IF SELF:_VFPDblClick != NULL
				SELF:_VFPDblClick:Call()
			ENDIF

		// ── MouseDown event ───────────────────────────────────────────────────
		PRIVATE _VFPMouseDown AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMouseDown AS STRING
			GET ; RETURN SELF:_VFPMouseDown?:SendTo ; END GET
			SET ; SELF:_VFPMouseDown := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireMouseDown() AS VOID STRICT
			IF SELF:_VFPMouseDown != NULL
				SELF:_VFPMouseDown:Call()
			ENDIF

		// ── MouseUp event ─────────────────────────────────────────────────────
		PRIVATE _VFPMouseUp AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMouseUp AS STRING
			GET ; RETURN SELF:_VFPMouseUp?:SendTo ; END GET
			SET ; SELF:_VFPMouseUp := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireMouseUp() AS VOID STRICT
			IF SELF:_VFPMouseUp != NULL
				SELF:_VFPMouseUp:Call()
			ENDIF

		// ── MouseMove event ───────────────────────────────────────────────────
		PRIVATE _VFPMouseMove AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMouseMove AS STRING
			GET ; RETURN SELF:_VFPMouseMove?:SendTo ; END GET
			SET ; SELF:_VFPMouseMove := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireMouseMove() AS VOID STRICT
			IF SELF:_VFPMouseMove != NULL
				SELF:_VFPMouseMove:Call()
			ENDIF

		// ── MouseEnter event ──────────────────────────────────────────────────
		PRIVATE _VFPMouseEnter AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMouseEnter AS STRING
			GET ; RETURN SELF:_VFPMouseEnter?:SendTo ; END GET
			SET ; SELF:_VFPMouseEnter := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireMouseEnter() AS VOID STRICT
			IF SELF:_VFPMouseEnter != NULL
				SELF:_VFPMouseEnter:Call()
			ENDIF

		// ── MouseLeave event ──────────────────────────────────────────────────
		PRIVATE _VFPMouseLeave AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMouseLeave AS STRING
			GET ; RETURN SELF:_VFPMouseLeave?:SendTo ; END GET
			SET ; SELF:_VFPMouseLeave := VFPOverride{NULL, VALUE} ; END SET
		END PROPERTY

		METHOD FireMouseLeave() AS VOID STRICT
			IF SELF:_VFPMouseLeave != NULL
				SELF:_VFPMouseLeave:Call()
			ENDIF

		// ── Refresh stub ──────────────────────────────────────────────────────
		// VFP Header.Refresh() redraws the header; no direct WinForms equivalent
		// for a cell header, so we invalidate the owning DataGridView if available.
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
