//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// DataGrid.prg
// This file contains subclasses from classes in System.Windows.Forms for the DataGrid Control
USING SWF := System.Windows.Forms

USING System.Windows.Forms
USING System.Drawing
USING System.Collections.Generic
USING VOSDK := XSharp.VO.SDK
CLASS VODataGridView INHERIT SWF.DataGridView IMPLEMENTS IVOControlProperties
	// We will activate the VirtualMode later and will then have to implement the methods
	// for Virtual Mode
	// See: http://msdn.microsoft.com/en-us/library/15a31akc%28v=vs.90%29.aspx
	PROTECT _lLastHScrollVisible AS LOGIC
	PROPERTY DataBrowser AS VOSDK.DataBrowser GET (VOSDK.DataBrowser) oProperties:Control

	#include "PropControlStyle.xh"

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		// Set Some defaults
		SELF:AllowUserToAddRows			 := FALSE
		SELF:AllowUserToDeleteRows		 := FALSE
		SELF:AllowUserToResizeRows		 := FALSE
		SELF:AllowUserToOrderColumns	 := TRUE
		SELF:AllowUserToResizeColumns	 := TRUE
		SELF:RowTemplate                 := VODataGridViewRow{}
		SELF:SelectionMode               := SWF.DataGridViewSelectionMode.CellSelect
		SELF:ScrollBars                  := SWF.ScrollBars.Horizontal
		SELF:EnableHeadersVisualStyles   := FALSE					// If this is TRUE then you cannot set color for column headers
		SELF:RowHeadersVisible           := FALSE
		SELF:ColumnHeadersHeightSizeMode := SWF.DataGridViewColumnHeadersHeightSizeMode.AutoSize
		SELF:TabStop					 := FALSE
		SELF:_lLastHScrollVisible := SELF:HorizontalScrollBar:Visible
		SELF:HorizontalScrollBar:VisibleChanged += HScrollVisibleChanged
		SELF:VisibleChanged += BrowserVisibilityChanged
		SELF:ShowCellToolTips := FALSE


	METHOD BrowserVisibilityChanged(o AS OBJECT, e AS EventArgs) AS VOID
		IF !SELF:IsDisposed .AND. !SELF:Disposing .AND. SELF:Visible
			SELF:DataBrowser:VScrollRepositionAndResize()
		ENDIF

	METHOD HScrollVisibleChanged(o AS OBJECT, e AS EventArgs) AS VOID
		IF !SELF:IsDisposed .AND. !SELF:Disposing .AND. SELF:_lLastHScrollVisible != SELF:HorizontalScrollBar:Visible
			SELF:DataBrowser:VScrollRepositionAndResize()
			SELF:_lLastHScrollVisible := SELF:HorizontalScrollBar:Visible
		ENDIF

	PROPERTY HScrollHeight AS LONG
		GET
			IF SELF:HorizontalScrollBar != NULL_OBJECT .and. SELF:HorizontalScrollBar:Visible
				RETURN SELF:HorizontalScrollBar:Height
			ELSE
				RETURN 0
			ENDIF

		END GET
	END PROPERTY

	ACCESS FirstRow AS VODataGridViewRow
		IF SELF:Rows:Count > 0
			RETURN (VODataGridViewRow) Rows[0]
		ENDIF
		RETURN NULL_OBJECT

	VIRTUAL PROTECTED METHOD OnKeyDown(e AS SWF.KeyEventArgs ) AS VOID
		// Handle special keys, rest is handled by parent class

		IF ! DataBrowser:ProcessKeyDown(e)
			SUPER:OnKeyDown(e)
		ENDIF


	PROTECTED METHOD CopyCellToClipboard() AS VOID
		IF SELF:CurrentCell != NULL_OBJECT
			LOCAL oCell AS SWF.DataGridViewCell
			LOCAL oEditControl AS SWF.Control
			LOCAL cCelldata AS STRING
			IF SELF:EditingControl != NULL
				oEditControl := SELF:EditingControl
				IF oEditControl IS SWF.TextBox VAR oTextbox
					cCelldata := oTextbox:SelectedText
				ELSE
					cCelldata := oEditControl:Text
				ENDIF
			ELSE
				oCell := SELF:CurrentCell
				cCelldata := (STRING) oCell:GetEditedFormattedValue(0, SWF.DataGridViewDataErrorContexts.ClipboardContent)
				cCelldata := alltrim(cCelldata)
			ENDIF
			IF (slen(cCelldata) > 0)
				SWF.Clipboard.SetText( cCelldata)
			ENDIF
		ENDIF
		RETURN

	VIRTUAL PROTECTED METHOD ProcessDataGridViewKey(e AS SWF.KeyEventArgs) AS LOGIC
		IF (e:KeyCode == SWF.Keys.C .and. e:Control)
			SELF:CopyCellToClipboard()
			RETURN TRUE
		ELSE
			RETURN SUPER:ProcessDataGridViewKey(e)
		ENDIF

	VIRTUAL PROTECTED METHOD ProcessCmdKey(msg REF SWF.Message, keyData AS SWF.Keys) AS LOGIC
		IF (int)keyData == 131139
			SELF:CopyCellToClipboard()
			RETURN TRUE
		ELSE
			RETURN SUPER:ProcessCmdKey(REF msg,keyData)
		ENDIF

	// The code below tries to paint vertical lines upto the bottom of the grid area.
	// It works, but not compeletely right when a horizontal scrollbar is visible
	/*
	VIRTUAL PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
		SUPER:OnPaint(e)
		LOCAL bottomRow := e:ClipRectangle:Bottom AS INT
		LOCAL rowHeight := SELF:RowTemplate:Height AS INT
        LOCAL h := SELF:ColumnHeadersHeight + rowHeight * SELF:RowCount +1 AS INT
        LOCAL imgWidth := SELF:Width - 2 AS INT
		LOCAL g := e:Graphics AS  Graphics
		// determine the width needed for the row headers
		LOCAL w := SELF:RowHeadersWidth  AS INT
		IF !SELF:RowHeadersVisible
			w := 1
		ENDIF
		// fill the area with the default back color
		LOCAL rFill  := Rectangle{w, h, imgWidth - 2, bottomRow-h} AS Rectangle
		g:FillRectangle(SolidBrush{SELF:DefaultCellStyle:BackColor}, rFill)
		// get all displayed columns and paint the right hand side lines
		LOCAL oCol AS DataGridViewColumn
		LOCAL pen := System.Drawing.Pen{SELF:GridColor, 1} AS System.Drawing.Pen
		oCol := SELF:Columns[SELF:FirstDisplayedScrollingColumnIndex]
		DO WHILE oCol != NULL_OBJECT
			w += oCol:Width
            g:DrawLine(pen, System.Drawing.Point{w, h}, System.Drawing.Point{w, bottomRow})
			oCol := SELF:Columns:GetNextColumn(oCol, DataGridViewElementStates.Visible, DataGridViewElementStates.None)
		ENDDO
		*/


		PROPERTY TopRowIndex AS LONG
			GET
				RETURN SUPER:FirstDisplayedScrollingRowIndex
			END GET
			SET
				IF Value >= 0 .and. Value < SELF:Rows:Count
					SUPER:FirstDisplayedScrollingRowIndex := Value
				ELSEIF Value > SELF:Rows:Count .and. SELF:Rows:Count > 0
					SUPER:FirstDisplayedScrollingRowIndex := SELF:Rows:Count-1
				ELSEIF SELF:Rows:Count > 0
					SUPER:FirstDisplayedScrollingRowIndex := 0
				ELSE
					// Do Nothing
                    NOP
				ENDIF
			END SET
		END PROPERTY

		METHOD ReverseSortOrder() AS VOID
			LOCAL aRows AS List<VODataGridViewRow>
			LOCAL aSelected AS LIST<LONG>
			LOCAL nCurrentCol AS LONG
			IF SELF:CurrentCell != NULL_OBJECT
				nCurrentCol := SELF:CurrentCell:ColumnIndex
			ELSE
				nCurrentCol := 0
			ENDIF
			SELF:SuspendLayout()
			aSelected := List<LONG>{}
			FOREACH oRow AS VODataGridViewRow IN SELF:SelectedRows
				aSelected:Add(oRow:Recno)
			NEXT
			aRows := List<VODataGridViewRow>{}
			FOREACH oRow AS VODataGridViewRow IN SELF:Rows
				aRows:Add(oRow)
			NEXT
			SELF:Rows:Clear()
			FOREACH oRow AS VODataGridViewRow IN aRows
				SELF:Rows:Insert(0, oRow)
				IF aSelected:Contains(oRow:Recno)
					oRow:Selected := TRUE
				ENDIF
			NEXT
			IF SELF:SelectedRows:Count > 0
				SELF:CurrentCell := SELF:SelectedRows[0]:Cells[nCurrentCol]
			ENDIF
			SELF:ResumeLayout()
			RETURN

		PROPERTY VisibleRows as LONG GET SUPER:DisplayedRowCount(FALSE)


END CLASS

CLASS VODataGridViewColumn INHERIT SWF.DataGridViewTextBoxColumn
	INTERNAL DataColumn AS DataColumn
	STATIC PROTECTED oForeColor AS System.Drawing.Color
	STATIC PROTECTED oBackColor AS System.Drawing.Color
	STATIC CONSTRUCTOR
		oForeColor := System.Drawing.SystemColors.ButtonFace
		oBackColor := System.Drawing.SystemColors.MenuText
	CONSTRUCTOR (oCol AS DataColumn)
		SUPER()
		SELF:DataColumn := oCol
		SELF:SortMode := SWF.DataGridViewColumnSortMode.Programmatic
		SELF:HeaderCell:Style:BackColor	:= oForeColor //VOSDK.Color{GuiWin32.GetSysColor(COLOR_BTNFACE)}
		SELF:HeaderCell:Style:ForeColor := oBackColor // VOSDK.Color{GuiWin32.GetSysColor(COLOR_BTNTEXT)}
		SELF:HeaderCell:Style:WrapMode  := SWF.DataGridViewTriState.False
		IF oCol:HyperLabel != NULL_OBJECT
			SELF:Name	:= oCol:HyperLabel:Name
		ENDIF



END CLASS


CLASS VODataGridViewRow INHERIT SWF.DataGridViewRow
	INTERNAL RecNo AS LONG

	PUBLIC METHOD GetRecNo() AS LONG
	RETURN RecNo
END CLASS
