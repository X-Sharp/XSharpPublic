//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This class emulates the VO Databrowser using the DotNet DataGridView
// Some remarks that may not be obvious to others
// The databrowser is attached to a server, but does NOT always include the current record of that server
// Simplified there are two situations:
// 1. When a record movement happens in the DataServer and Notification is enabled, then the DataBrowser
//    will receive a notification and will try to set focus to the row that matches the current record in the databrowser
//    If that row is not in the current buffer then the buffer will be refreshed and the row with the current record
//	  will be selected
// 2. When the user uses the mouse wheel and/or vertical scrollbar then the display of the DataBrowser is updated
//    but the current record in the server is NOT changed. Repeatedly scrolling up or down may lead to a situation where
//    the end of the current buffer is reached. Then the databrowser will add new rows to the buffer but will not move the
//    focus to the current record in the server.
//	  A complication in this scenario is that to fill the buffer the Databrowser needs to move the server, but afterwards the
//    current record in the server will be always restored. To fill the buffer the DataBrowser therefore disables notifications
//    on the server.
//
// After scrolling a databrowser and then Switching from Browse View to Form View and then back it is therefore also possible
// that the current row in the server is not available in the browser. That is exactly the case in VO.
//
// Some remarks:
// The "View" is the subset of rows in the DataGridView Buffer. It does not have to have the current record in the DataServer
// The scrollbar shows the position of the current selected row when it is visible and otherwise the first row on the screen




USING System.Runtime.InteropServices
USING System.Reflection
USING System.Collections.Generic
USING System.Drawing
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK

/// <include file="Gui.xml" path="doc/DataBrowser/*" />
class DataBrowser inherit VOSDK.Control implements IDataBrowser
	PROTECT iBufferGranularity AS INT
	PROTECT iBufferMaximum AS INT
	PROTECT iDeferPaintCount   AS INT
	PROTECT iDeferNotifyCount  AS INT
	PROTECT iRecordSize        AS INT
	PROTECT nOldRecordNum      AS INT

	PROTECT dwDeferredStyle    AS DWORD

	PROTECT lCaptionSet        AS LOGIC
	PROTECT lColumnTitles      AS LOGIC
	PROTECT lIsReadOnly        AS LOGIC
	PROTECT lIsShown           AS LOGIC
	PROTECT lHasTop            AS LOGIC
	PROTECT lHasBottom         AS LOGIC
	PROTECT lUseDefCaption     AS LOGIC
	PROTECT lUseDefColCaption  AS LOGIC
	PROTECT lUseDefText        AS LOGIC
	PROTECT lUseDefButton      AS LOGIC
	PROTECT lUseDefColButton   AS LOGIC
	PROTECT lUseDefHiText      AS LOGIC
	PROTECT lUse3dLook         AS LOGIC
	PROTECT lLinked            AS LOGIC

	PROTECT aColumn            AS ARRAY
	PROTECT nFocusField	       AS LONG
	PROTECT oDataServer        AS DataServer
	PROTECT oTextPointer       AS Pointer
	PROTECT VScrollBar		   AS VScrollBar
	INTERNAL oScrollBarManager   AS DataBrowserScrollBarManager
	PROTECT iScrollOffSet		AS LONG
	PROTECT lInEvent			AS LONG
	PROTECT oRowDict			AS System.Collections.Generic.Dictionary<INT, VODataGridViewRow>
	PROTECT oVOEditControl		AS SingleLineEdit


	INTERNAL PROPERTY HasTop	AS LOGIC GET lHasTop
	INTERNAL PROPERTY HasBottom AS LOGIC GET lHasBottom

	#region Obsolete Methods



	#endregion

	PROPERTY ControlType AS ControlType GET ControlType.DataBrowser

	METHOD OnControlCreated(oC AS IVOControl) AS VOID
		LOCAL oGrid AS VODataGridView
		oGrid := (VODataGridView) oC
		// Event Handlers
		oGrid:RowEnter                     +=  OnRowEnter
		oGrid:CellEndEdit                  += OnCellEndEdit
		oGrid:SortCompare                  += OnSortCompare
		oGrid:MouseWheel	                += OnMouseWheel
		oGrid:ColumnHeaderMouseClick       += OnColumnHeaderMouseClick
		oGrid:ColumnHeaderMouseDoubleClick += OnColumnHeaderMouseDoubleClick
		oGrid:EditingControlShowing        += OnEditingControlShowing
		oGrid:CellDoubleClick              += OnCellDoubleClick
		oGrid:VisibleChanged               += OnVisibleChanged
		oGrid:HandleCreated                += DataBrowserHandleCreated
		oGrid:CellEnter					   += OnCellEnter

		//oC:CellPainting					+= OnCellPainting

		// Colors
		oGrid:BackColor							:= System.Drawing.SystemColors.Window
		oGrid:GridColor							:= System.Drawing.SystemColors.WindowFrame
		oGrid:BackgroundColor						:= System.Drawing.SystemColors.AppWorkspace

		oGrid:DefaultCellStyle:BackColor			:= System.Drawing.SystemColors.Window
		oGrid:DefaultCellStyle:ForeColor			:= System.Drawing.SystemColors.WindowText
		oGrid:DefaultCellStyle:SelectionBackColor	:= System.Drawing.SystemColors.Highlight
		oGrid:DefaultCellStyle:SelectionForeColor	:= System.Drawing.SystemColors.HighlightText
		oGrid:DefaultCellStyle:Font				:= System.Drawing.SystemFonts.DefaultFont

		oGrid:ColumnHeadersDefaultCellStyle:BackColor	:= System.Drawing.SystemColors.MenuBar
		oGrid:ColumnHeadersDefaultCellStyle:ForeColor	:= System.Drawing.SystemColors.MenuText
		oGrid:ColumnHeadersDefaultCellStyle:Font		:= System.Drawing.SystemFonts.DefaultFont

		oGrid:ColumnHeadersBorderStyle				:= DataGridViewHeaderBorderStyle.Raised
		oGrid:ColumnHeadersHeightSizeMode			:= DataGridViewColumnHeadersHeightSizeMode.DisableResizing
		//oC:ColumnHeadersHeight					:= 12
		oGrid:SelectionMode						:= DataGridViewSelectionMode.FullRowSelect

		// Setup OUR vertical scrollbar. We do not use the standard vertical scroll bar
		SELF:VScrollBar			:= VScrollBar{}
		SELF:VScrollBar:Parent	:= oGrid
		SELF:oScrollBarManager	:= DataBrowserScrollBarManager{SELF, SELF:VScrollBar, oGrid}

		RETURN


	#region Vertical Scrollbar operation
	// Berechnet die Position und die Größe der Custom VScrollbar neu. Wird momentan angerufen, wenn sich die sichtbarkeit der HScroll oder die Sichtbarkeit ändert, und wenn die Gridview sichtbar wird
	PUBLIC METHOD VScrollRepositionAndResize() AS VOID
		LOCAL oSize AS System.Drawing.Size
		LOCAL nVWidth, nControlHeight, nControlWidth AS INT
		IF SELF:__DataGridView != NULL .AND. SELF:VScrollBar!=NULL
			oSize := SELF:__DataGridView:Size
			nControlHeight	:= oSize:Height
			nControlWidth	:= oSize:Width
			nVWidth			:= System.Windows.Forms.SystemInformation.VerticalScrollBarWidth
			nControlHeight	-= __DataGridView:HScrollHeight
			SELF:VScrollBar:Size		:= System.Drawing.Size{nVWidth,nControlHeight-2}
			SELF:VScrollBar:Location	:= System.Drawing.Point{nControlWidth-nVWidth-1,2}
			SELF:VScrollBar:Anchor		:= System.Windows.Forms.AnchorStyles.Right | System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom
			SELF:VScrollBar:LargeChange := SELF:__DataGridView:VisibleRows
		ENDIF

	PRIVATE METHOD VScrollBarShowWhenNeeded() AS VOID
		// This method calculates the size of the scrollbar and moves the thumb position.
		// Note that the scrollbar does NOT show the current row in the server but shows which part of the result set is visible
		// Because we are not loading all the rows we check for lHasTop and lHasBottom and add a number of 1/8 of the buffer granularity
		// at each side when needed.
		LOCAL lHidden AS LOGIC
		IF SELF:__DataGridView != NULL_OBJECT
			SELF:VScrollRepositionAndResize()
			lHidden := FALSE
			IF SELF:__IsDataServerEmpty()
				lHidden := TRUE
			ELSE
				IF lHasTop .and. lHasBottom
					IF SELF:__DataGridView:Rows:Count <= SELF:__DataGridView:VisibleRows
						// No need for a scrollbar
						lHidden := TRUE
					ENDIF
				ENDIF
			ENDIF
			SELF:VScrollBar:Visible := ! lHidden

		ENDIF
		RETURN

	PRIVATE METHOD SetVScrollBar()AS VOID
		LOCAL oRow AS VODataGridViewRow
		IF SELF:oDataServer != NULL
			oRow := SELF:__GetRowAtRecNo(SELF:oDataServer:Recno)
			IF oRow != NULL_OBJECT .and. oRow:Index >= 0
				oRow:Selected := TRUE
			ENDIF
			SELF:oScrollBarManager:SyncScrollBar()
		END
		RETURN


	#endregion

	#region Event Handlers


	PRIVATE METHOD OnMouseWheel( sender AS OBJECT, e AS MouseEventArgs) AS VOID
		LOCAL currentIndex	AS LONG
		LOCAL ScrollLines	AS LONG
		currentIndex := __DataGridView:TopRowIndex
		ScrollLines  := SystemInformation.MouseWheelScrollLines
		IF e:Delta > 0
			IF currentIndex > ScrollLines
				__DataGridView:TopRowIndex := currentIndex - ScrollLines
			ELSEIF lHasTop
				__DataGridView:TopRowIndex := 0
			ELSE
				SELF:__DeltaBuildBufferUp()
				currentIndex := __DataGridView:TopRowIndex
				IF currentIndex > ScrollLines
					__DataGridView:TopRowIndex := currentIndex - ScrollLines
				ELSE
					__DataGridView:TopRowIndex := 0
				ENDIF
			ENDIF
		ELSEIF e:Delta < 0
			IF  currentIndex >= __DataGridView:Rows:Count - VScrollBar:LargeChange -1
				IF ! SELF:lHasBottom
					SELF:__DeltaBuildBufferDown()
					__DataGridView:TopRowIndex := currentIndex + ScrollLines
				ENDIF
			ELSE
				__DataGridView:TopRowIndex := currentIndex + ScrollLines
			ENDIF
		ENDIF
		SELF:SetVScrollBar()
		RETURN

	METHOD OnCellEnter(sender AS OBJECT, e AS DataGridViewCellEventArgs) AS VOID
		LOCAL nIndex AS LONG
		nIndex := e:ColumnIndex
		IF nIndex != SELF:nFocusField
			SELF:__NewFocusField(nIndex)
		ENDIF
		RETURN

	METHOD DataBrowserHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:__IsValid
			GuiWin32.SetWindowText(oCtrl:Handle, "DataBrowser")
		ENDIF

	METHOD OnCellDoubleClick(sender AS OBJECT, e AS DataGridViewCellEventArgs) AS VOID
		IF e:RowIndex >= 0
			SELF:CellDoubleClick()
		ENDIF
		RETURN

	METHOD OnColumnHeaderMouseClick(sender AS OBJECT, e AS DataGridViewCellMouseEventArgs) AS VOID STRICT
		IF SELF:__IsValid .and. __DataGridView:CurrentCell != NULL_OBJECT
			IF __DataGridView:CurrentCell:ColumnIndex != e:ColumnIndex
				__DataGridView:CurrentCell := __DataGridView:Rows[__DataGridView:CurrentCell:RowIndex]:Cells[e:ColumnIndex]
			ENDIF
		ENDIF
		RETURN

	METHOD OnColumnHeaderMouseDoubleClick(sender AS OBJECT, e AS DataGridViewCellMouseEventArgs) AS VOID STRICT
		LOCAL oCol AS DataColumn
		oCol := SELF:GetColumn(e:ColumnIndex+1)
		SELF:CellCaptionDoubleClick( oCol )
		RETURN

	PROTECTED METHOD OnSortCompare(sender AS OBJECT, e AS DataGridViewSortCompareEventArgs) AS VOID
		LOCAL oRow1 AS VODataGridViewRow
		LOCAL oRow2 AS VODataGridViewRow
		IF SELF:__IsValid
			e:Handled := TRUE
			oRow1 := (VODataGridViewRow) __DataGridView:Rows[e:RowIndex1]
			oRow2 := (VODataGridViewRow) __DataGridView:Rows[e:RowIndex2]
			e:SortResult := oRow1:RecNo - oRow2:RecNo
			e:Handled := TRUE
		ENDIF
		RETURN

	VIRTUAL PROTECTED METHOD OnEditingControlShowing(sender AS OBJECT, e AS DataGridViewEditingControlShowingEventArgs) AS VOID
		LOCAL chilf AS STRING
		IF SELF:CurrentColumn != NULL .AND. TypeOf(System.Windows.Forms.TextBox):isAssignableFrom(e:Control:GetType())
            e:Control:PreviewKeyDown += OnEditControlPreviewKeyDown
			self:oVOEditControl := SingleLineEdit{e:Control}
			SELF:oVOEditControl:RegisterEvents(e:Control)
			chilf := SELF:oVOEditControl:TextValue
			SELF:oVOEditControl:FieldSpec := SELF:CurrentColumn:FieldSpec
			SELF:oVOEditControl:TextValue := chilf
		ENDIF
		RETURN

	PROTECT lInRowEnter AS LOGIC
	VIRTUAL PROTECTED METHOD OnRowEnter(sender AS OBJECT, e AS DataGridViewCellEventArgs) AS VOID
		LOCAL oRow AS VODataGridViewRow
		IF ! lInRowEnter
			TRY
				lInRowEnter := TRUE
				lInEvent += 1
				oRow := (VODataGridViewRow) __DataGridView:Rows[e:RowIndex]
				IF oRow != NULL_OBJECT
					SELF:__NewFocusRecord(oRow)
				ENDIF
				SELF:SetVScrollBar()
			FINALLY
				lInEvent -= 1
				lInRowEnter := FALSE
			END TRY
		ENDIF
		RETURN


	VIRTUAL PROTECTED METHOD OnEditControlPreviewKeyDown(sender AS OBJECT, e AS PreviewKeyDownEventArgs) AS VOID
		DO CASE
		CASE e:Control .or. e:Alt .or. e:Shift
			// Do nothing
            NOP
		CASE e:KeyCode == Keys.Escape
			e:IsInputKey := TRUE
			SELF:__EndEditField(0)
		ENDCASE
		RETURN

	VIRTUAL PROTECTED METHOD OnCellEndEdit(sender AS OBJECT, e AS DataGridViewCellEventArgs) AS VOID
		TRY
			lInEvent += 1
			// Das __DataGridView:EditingControl ist an dieser Stelle schon NULL, aus diesem Grund speichere ich die Referenz zwischen
			IF SELF:oVOEditControl != NULL
				SELF:oVOEditControl:__Control:PreviewKeyDown -= OnEditControlPreviewKeyDown
				SELF:oVOEditControl:UnRegisterEvents()
				SELF:oVOEditControl := NULL
			ENDIF
			SELF:__SaveEditChanges()
		FINALLY
			lInEvent -= 1
		END TRY
		RETURN

	VIRTUAL PROTECTED METHOD OnVisibleChanged(sender AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:__IsValid
			IF SELF:__DataGridView:Visible
				SELF:__RecordChange()
			ENDIF
		ENDIF

	#endregion
	PRIVATE METHOD Skipper(nRows AS LONG) AS VOID
		SELF:__SkipRaw(nRows)
		IF nRows > 0 .and. SELF:oDataServer:EOF
			IF ! SELF:oDataServer:BOF
				SELF:oDataServer:GoBottom()
			ENDIF
		ENDIF
		IF nRows < 0 .and. SELF:oDataServer:BOF
			SELF:oDataServer:GoTop()
		ENDIF
		SELF:SetVScrollBar()


	#region Callbacks
	VIRTUAL METHOD ProcessKeyDown(e AS KeyEventArgs) AS LOGIC
		// This method is called from VODataGridView:OnKeyDown and allows to override the standard keyboard handlers
		// We override the standard behavior so we can skip outside the loaded rows
		LOCAL lMustSkip AS LOGIC
		LOCAL nRowIndex := -1 AS LONG
		IF SELF:oDataServer != NULL_OBJECT .and. SELF:__IsValid
			IF __DataGridView:CurrentRow != NULL_OBJECT
				nRowIndex := __DataGridView:CurrentRow:Index
			ELSE
				nRowIndex := -1
			ENDIF
			DO CASE
			CASE e:Handled
				e:Handled := TRUE
			CASE e:Control
				e:Handled := TRUE
				SWITCH e:KeyCode
				CASE Keys.Home
					SELF:oDataServer:GoTop()
				CASE Keys.PageUp
					SELF:oDataServer:GoTop()
				CASE Keys.End
					SELF:oDataServer:GoBottom()
				CASE Keys.PageDown
					SELF:oDataServer:GoBottom()
				OTHERWISE
					e:Handled := FALSE
				END SWITCH
			CASE e:KeyCode == Keys.Up
				// Only skip when we have not seen the first of the records
				// Otherwise we let the browser handle the skip
				// The OnRowEnter event will then synchronize the row with the server
				IF SELF:lHasTop
					lMustSkip := FALSE
				ELSE
					IF nRowIndex > 0
						lMustSkip := FALSE
					ELSE
						lMustSkip := TRUE
					ENDIF
				ENDIF
				IF lMustSkip
					SELF:Skipper(-1)
					e:Handled := TRUE
				ENDIF
			CASE e:KeyCode == Keys.Down
				// Only skip when we have not seen the last of the records
				// Otherwise we let the browser handle the skip
				// The OnRowEnter event will then synchronize the row with the server
				IF SELF:lHasBottom
					lMustSkip := FALSE
				ELSE
					IF nRowIndex >= 0  .and. nRowIndex < __DataGridView:Rows:Count -1
						lMustSkip := FALSE
					ELSE
						lMustSkip := TRUE
					ENDIF
				ENDIF
				IF lMustSkip
					SELF:Skipper(1)
					e:Handled := TRUE
				ENDIF
			CASE e:KeyCode == Keys.PageUp
				// Only skip when we have not seen the first of the records
				// Otherwise we let the browser handle the skip
				// The OnRowEnter event will then synchronize the row with the server
				IF SELF:lHasTop
					lMustSkip := FALSE
				ELSE
					IF nRowIndex > SELF:__DataGridView:VisibleRows
						lMustSkip := FALSE
					ELSE
						lMustSkip := TRUE
					ENDIF
				ENDIF
				IF lMustSkip
					SELF:Skipper(- SELF:__DataGridView:VisibleRows)
					e:Handled := TRUE
				ENDIF
			CASE e:KeyCode == Keys.PageDown
				// Only skip when we have not seen the last of the records
				// Otherwise we let the browser handle the skip
				// The OnRowEnter event will then synchronize the row with the server
				IF SELF:lHasBottom
					lMustSkip := FALSE
				ELSE
					IF (nRowIndex >= 0 .and. nRowIndex < __DataGridView:Rows:Count - SELF:__DataGridView:VisibleRows)
						// Let the gridview handle it
						lMustSkip := FALSE
					ELSE
						lMustSkip := TRUE
					ENDIF
				ENDIF
				IF lMustSkip
					SELF:Skipper(+ SELF:__DataGridView:VisibleRows)
					e:Handled := TRUE
				ENDIF
			CASE e:KeyCode == Keys.Enter
				SELF:CellDoubleClick()
				e:Handled := TRUE
			ENDCASE
			SELF:SetVScrollBar()
		ENDIF
		RETURN	e:Handled
	#endregion

	ACCESS __HasColumns AS LOGIC
		RETURN SELF:__IsValid .and. __DataGridView:Columns:Count > 0

	ACCESS __DataGridView AS VODataGridView
		IF oCtrl == NULL_OBJECT
			SELF:Create()
		ENDIF
		RETURN (VODataGridView) oCtrl


	 /// <exclude />
	METHOD __AddColumn (oDataColumn AS DataColumn, iCol AS INT) AS LOGIC STRICT
		LOCAL cCaption AS STRING
		LOCAL oDC AS DataColumn
		LOCAL oColumn AS DataGridViewColumn

		LOCAL lRC AS LOGIC
		IF oDataColumn == NULL_OBJECT
			WCError{#AddColumn,#DataBrowser,__WCSTypeError,oDataColumn,1}:Throw()
		ENDIF

		oDC := oDataColumn
		IF (oDC:Owner != NULL_OBJECT) //if DataColumn already assigned to Browser
			RETURN FALSE
		ENDIF

		oColumn := oDataColumn:oDataGridColumn

		IF oDC:dwWidth == 0XFFFFFFFF
			oDC:dwWidth := 16
		ELSEIF (oDataColumn:FieldSpec != NULL_OBJECT) .AND. (oDataColumn:FieldSpec:ValType == "O")
			oDC:dwWidth:= 32
		ENDIF

		cCaption			:= oDC:GetCaption()
		oColumn:HeaderText 	:= cCaption
		oColumn:Name 		:= oDataColumn:Name

		// if data server is present - write out and delete internal buffers
		IF (oDataServer != NULL_OBJECT)
			SELF:__ClearBuffers()
		ENDIF

		oDC:__Owner := (DataBrowser) (OBJECT) SELF

		IF (iCol == 0)
			lRC := (SELF:__DataGridView:Columns:Add(oColumn) >= 0)
		ELSE
			SELF:__DataGridView:Columns:Insert(iCol, oColumn)		// Not -1, because we start with a Recno column
			lRC := TRUE
		ENDIF

		IF (lRC)
			// Insert/Add Column to array
			IF (iCol != 0)
				Ains(aColumn, (DWORD) iCol)
				aColumn[iCol] := oDC
			ELSE
				aadd(aColumn, oDC)
			ENDIF
		ENDIF

		RETURN lRC

 /// <exclude />
	method __AutoLayout() as void strict
		LOCAL iFields, iStart, iBegin, iEnd AS INT
		LOCAL iStep AS INT
		LOCAL aNewColumns AS ARRAY
		LOCAL oDataField AS DataField
		LOCAL oNewColumn AS DataColumn
		LOCAL oPropFS AS FieldSpec
		LOCAL oPropHL AS HyperLabel
		LOCAL oWindow AS Window
		LOCAL oDatawin  AS DataWindow

		aNewColumns:={}

		IF lLinked
			iFields := (INT) oDataServer:FCount

			IF IsBiDi()
				iBegin := iFields
				iEnd := 1
				iStep := -1
			ELSE
				iBegin := 1
				iEnd := iFields
				iStep := 1
			ENDIF

			oWindow := (Window) SELF:Owner

			IF oWindow IS DataWindow //vor die Loop gesetzt, da sich das Ergebnis in der Loop nicht ändert
				oDatawin := (DataWindow) oWindow
			ELSE
				oDatawin := NULL_OBJECT
			ENDIF

			FOR iStart := iBegin TO iEnd STEP iStep
				oDataField := oDataServer:DataField(iStart)

				IF oDataField == NULL_OBJECT
					LOOP
				ENDIF

				// If there are explicit properties in the DataWindow view
				// propagate them, else use the DataField Properties
				IF oDatawin == NULL_OBJECT
					oPropHL := oDataField:HyperLabel
					oPropFS := oDataField:FieldSpec
				ELSE
					IF (oPropHL := oDatawin:__FindHyperLabel(oDataField:NameSym)) == NULL_OBJECT
						oPropHL := oDataField:HyperLabel
					ENDIF
					IF (oPropFS := oDatawin:__FindFieldSpec(oDataField:NameSym)) == NULL_OBJECT
						oPropFS := oDataField:FieldSpec
					ENDIF
				ENDIF
				oNewColumn := DataColumn{oPropFS, oPropHL}
				oNewColumn:Caption := __GetDFCaption(oDataField,{})
				oNewColumn:LinkDF(oDataServer, iStart)
				AADD(aNewColumns,oNewColumn)
			NEXT
			IF ALen(aNewColumns)     > 0
				SELF:AddColumn(aNewColumns)
			ENDIF

		ENDIF
		RETURN



[Obsolete];
	METHOD __AutoResize() AS VOID STRICT
		// Handled inside DataForm Class
		RETURN


	[Obsolete];
	METHOD __BeginEditField(hWin AS IntPtr, dwChar AS DWORD) AS VOID STRICT
		RETURN

/// <exclude />
	METHOD __BuildBuffer() AS VOID STRICT
		LOCAL iRecNo AS INT
		LOCAL nCol    AS LONG
		LOCAL oRow		AS VODataGridViewRow
		IF SELF:__HasColumns .and. SELF:lInEvent == 0

			IF __DataGridView:CurrentCell != NULL_OBJECT
				nCol := __DataGridView:CurrentCell:ColumnIndex
			ENDIF

			IF oDataServer != NULL_OBJECT
				TRY
					SELF:__CursorWait()

					SELF:SuspendUpdate()
					SELF:__DeferNotify()

					SELF:__ClearBuffers()
					IF !SELF:__IsDataServerEmpty()
						iRecNo := oDataServer:RecNo

						SELF:__BuildNewBuffer(iRecNo)
						IF oDataServer:RecNo != iRecNo
							oDataServer:GoTo(iRecNo)
						ENDIF

						oRow := SELF:__GetRowAtRecNo(iRecNo)

						IF oRow != NULL_OBJECT
							nCol := SELF:__ValidateFocusColNo(nCol)
							IF nCol >= 0
								__DataGridView:CurrentCell := oRow:Cells[nCol]
							ENDIF
							IF !SELF:__IsFocusRecordInView()
								SELF:__FocusToCurrentRow()
							ENDIF
						ELSE
							// No rows selected
							//SELF:__DataGridView:SelectedRows:Clear()
                            NOP

						ENDIF

					ENDIF
					SELF:SetVScrollBar()
				FINALLY
					SELF:__EnableNotify()
					SELF:RestoreUpdate()
					SELF:__CursorRestore()
				END TRY
			ENDIF
		ENDIF
		RETURN



/// <exclude />
	METHOD __BuildNewBuffer(iRecNo AS INT) AS VOID STRICT
		LOCAL oFocusRow AS VODataGridViewRow
		LOCAL oRow AS VODataGridViewRow
		LOCAL nCol AS INT
		IF SELF:__HasColumns .and. SELF:lInEvent == 0
			IF __DataGridView:CurrentCell != NULL_OBJECT
				nCol := __DataGridView:CurrentCell:ColumnIndex
			ENDIF

			IF oDataServer != NULL_OBJECT
				TRY
					SELF:SuspendUpdate()
					SELF:__DeferNotify()
					IF oDataServer:RecNo != iRecNo
						oDataServer:GoTo(iRecNo)
					ENDIF

					lHasTop		:= FALSE
					lHasBottom	:= FALSE
					lHasTop		:= SELF:__TestForTop()
					DO WHILE (__DataGridView:RowCount <= iBufferGranularity)
						IF oDataServer:EoF
							EXIT
						ENDIF
						oRow := SELF:__GetRowAtRecNo(oDataServer:Recno)
						IF oRow == NULL_OBJECT
							oRow := SELF:__BuildRecord(TRUE)
						ENDIF
						IF (oRow == NULL_OBJECT)
							EXIT
						ENDIF

						oDataServer:Skip(1)
					ENDDO

					lHasBottom	:= SELF:__TestForBottom()
					oFocusRow	:= oRow
					IF (__DataGridView:RowCount < iBufferGranularity .and. ! lHasTop)
						oDataServer:GoTo(iRecNo)
						SELF:__SkipRaw(-iBufferGranularity)
						SELF:__BuildRowsAtTopOfBuffer()
					ENDIF
					SELF:VScrollBarShowWhenNeeded()
					IF oFocusRow != NULL_OBJECT
						oRow := SELF:__GetRowAtRecNo(oFocusRow:Recno)
						nCol := SELF:__ValidateFocusColNo(nCol)
						IF nCol >= 0
							__DataGridView:CurrentCell := oRow:Cells[nCol]
						ENDIF
					ENDIF
				FINALLY
					SELF:__EnableNotify()
					SELF:RestoreUpdate()
				END TRY
			ENDIF
		ENDIF
		RETURN


/// <exclude />
	METHOD __BuildRecord(lInsertAtEnd AS LOGIC) AS VODataGridViewRow STRICT
		LOCAL oRow AS VODataGridViewRow
		LOCAL iRow AS INT
		LOCAL iRecNo AS INT
		IF (oDataServer != NULL_OBJECT) .and. SELF:__IsValid
			iRecNo := oDataServer:RecNo
			oRow := SELF:__GetRowAtRecNo(iRecNo)
			IF oRow == NULL_OBJECT
				IF lInsertAtEnd
					iRow := SELF:__DataGridView:Rows:Add()
				ELSE
					SELF:__DataGridView:Rows:Insert(0,1)
					iRow := 0
				ENDIF
				IF (iRow >= 0)
					oRow := (VODataGridViewRow) SELF:__DataGridView:Rows[iRow]
					oRow:RecNo := iRecNo
					IF !oRowDict:ContainsKey(iRecNo)
						oRowDict:Add(iRecNo, oRow)
					ENDIF
					SELF:__FillRecord(oRow)
					IF lIsReadOnly
						oRow:ReadOnly := TRUE
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		RETURN oRow



	[Obsolete];
	METHOD __BuildRecordDescription() AS LOGIC STRICT
		RETURN TRUE
 /// <exclude />
	METHOD __BuildRowsAtTopOfBuffer() AS VOID STRICT
		LOCAL aRows AS List<VODataGridViewRow>
		LOCAL oRow AS VODataGridViewRow
		LOCAL nRec	AS LONG
		IF ! SELF:__IsValid
			RETURN
		ENDIF
		aRows :=  List<VODataGridViewRow>{}
		FOREACH oGridRow AS VODataGridViewRow IN SELF:__DataGridView:Rows
			aRows:Add(oGridRow)
		NEXT
		SELF:__DataGridView:Rows:Clear()
		SELF:oRowDict:Clear()
		nRec :=oDataServer:Recno
		DO WHILE (__DataGridView:RowCount <= iBufferGranularity)
			IF oDataServer:EoF
				EXIT
			ENDIF
			oRow := SELF:__GetRowAtRecNo(oDataServer:Recno)
			IF oRow == NULL_OBJECT
				oRow := SELF:__BuildRecord(TRUE)
			ENDIF
			IF (oRow == NULL_OBJECT)
				EXIT
			ENDIF

			oDataServer:Skip(1)
		ENDDO
		oDataServer:Goto(nRec)
		//Restore missing rows in the buffer
		FOREACH oSavedRow AS VODataGridViewRow IN aRows
			IF __DataGridView:Rows:Count >= iBufferMaximum
				EXIT
			ENDIF
			oRow := SELF:__GetRowAtRecNo(oSavedRow:Recno)
			IF oRow == NULL_OBJECT
				// Row does not exist, so add back to the buffer
				SELF:__DataGridView:Rows:Add(oSavedRow)
				IF !oRowDict:ContainsKey(oSavedRow:Recno)
					oRowDict:Add(oSavedRow:Recno, oSavedRow)
				ENDIF
			ENDIF
		NEXT
	RETURN

 /// <exclude />
	METHOD __SkipRaw(nToSkip AS LONG) AS VOID
		oDataServer:Skip( nToSkip )
		RETURN


 /// <exclude />
	METHOD __ClearBuffers() AS VOID STRICT
		IF SELF:__HasColumns .and. SELF:lInEvent = 0
			SELF:__EndEditField(0)
			IF SELF:RowCount > 0
				SELF:__DataGridView:Rows:Clear()
			ENDIF
			oRowDict:Clear()
			lHasTop		:= TRUE
			lHasBottom	:= TRUE
		ENDIF
		RETURN
 /// <exclude />
	ACCESS __CurrentRow AS VODataGridViewRow
		LOCAL nCurrentRow AS LONG
		IF SELF:__HasColumns .and. SELF:lInEvent = 0
			IF SELF:RowCount > 0 .and.  __DataGridView:CurrentCell != NULL_OBJECT
				nCurrentRow := __DataGridView:CurrentCell:RowIndex
				RETURN (VODataGridViewRow) SELF:__DataGridView:Rows[nCurrentRow]
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

 /// <exclude />
METHOD __CursorWait() AS VOID STRICT
		__DataGridView:Cursor := System.Windows.Forms.Cursors.WaitCursor
		RETURN
 /// <exclude />
	METHOD __CursorRestore() AS VOID STRICT
		__DataGridView:Cursor := System.Windows.Forms.Cursors.Default
		RETURN


 /// <exclude />
	METHOD __DeferNotify() AS VOID STRICT
		IF iDeferNotifyCount==0 .AND. oDataServer != NULL_OBJECT
			oDataServer:SuspendNotification()
		ENDIF
		iDeferNotifyCount := iDeferNotifyCount + 1
		RETURN


	[Obsolete];
	METHOD __DeltaBuildBuffer() AS VOID STRICT
		RETURN
 /// <exclude />
	METHOD __DeltaBuildBufferDown() AS VOID STRICT
		// Extend the view with rows at the end
		// Does not move the server before filling
		LOCAL i AS INT
		LOCAL oRow  AS VODataGridViewRow
		LOCAL nRecord  AS LONG
		IF !SELF:__IsValid
			RETURN
		ENDIF
		TRY
			SELF:__CursorWait()
			SELF:SuspendUpdate()
			SELF:__DeferNotify()
			IF __DataGridView:Rows:Count > 0
				oRow := (VODataGridViewRow) SELF:__DataGridView:Rows[SELF:__DataGridView:Rows:Count-1]
				oDataServer:GoTo(oRow:RecNo)
				oDataServer:Skip(1)
			ENDIF
			FOR i:=1 UPTO iBufferGranularity
				IF oDataServer:EOF
					SELF:lHasBottom := TRUE
					EXIT
				ENDIF
				nRecord := oDataServer:Recno
				oRow := SELF:__GetRowAtRecNo(nRecord)
				IF oRow == NULL_OBJECT
					oRow := SELF:__BuildRecord(TRUE)
					IF (oRow == NULL_OBJECT)
						EXIT
					ENDIF
				ENDIF
				oDataServer:Skip(1)
			NEXT

			DO WHILE __DataGridView:Rows:Count > iBufferMaximum
				oRow := (VODataGridViewRow) SELF:__DataGridView:Rows[0]
				SELF:__DataGridView:Rows:Remove(oRow)
				oRowDict:Remove(oRow:Recno)
				lHasTop := FALSE
			ENDDO

			lHasBottom	:= SELF:__TestForBottom()
		FINALLY
			SELF:VScrollBarShowWhenNeeded()
			SELF:__EnableNotify()
			SELF:RestoreUpdate()
			SELF:__CursorRestore()
		END TRY
		RETURN


 /// <exclude />
	METHOD __DeltaBuildBufferUp() AS VOID STRICT
		// Extend the view with rows at the beginning
		LOCAL iRecno AS INT						// Record Number of row with focus
		LOCAL iOldRec   AS INT					// Current record number in the Server
		LOCAL oRow AS VODataGridViewRow
		IF !SELF:__IsValid
			RETURN
		ENDIF
		TRY
			SELF:__CursorWait()
			SELF:SuspendUpdate()
			SELF:__DeferNotify()
			oRow	:= (VODataGridViewRow) SELF:__DataGridView:Rows[0]
			iRecno	:= oRow:RecNo
			iOldRec := oDataServer:Recno
			oDataServer:GoTo(iRecno)
			SELF:__SkipRaw(-iBufferGranularity)
			IF oDataServer:BOF
				oDataServer:GoTop()
				SELF:lHasTop := TRUE
			ENDIF
			SELF:__BuildRowsAtTopOfBuffer()
			oDataServer:GoTo(iOldRec)
			oRow := SELF:__GetRowAtRecNo(iRecno)
			IF oRow != NULL_OBJECT
				SELF:__DataGridView:TopRowIndex := oRow:Index
			ELSE
				SELF:__DataGridView:TopRowIndex := 0
			ENDIF
		FINALLY
			SELF:VScrollBarShowWhenNeeded()
			SELF:__EnableNotify()
			SELF:RestoreUpdate()
			SELF:__CursorRestore()
		END TRY
		RETURN



 /// <exclude />
    [Obsolete];
METHOD __DeltaRebuildBufferDown() AS VOID STRICT


 /// <exclude />
    [Obsolete];
METHOD __DeltaRebuildBufferUp() AS VOID STRICT

/// <exclude />
	[Obsolete];
	METHOD __DestroyEditField() AS VOID STRICT
		RETURN
/// <exclude />
    [Obsolete];
	METHOD __DrawCellData(hDC AS IntPtr, iX AS INT, iY AS INT, dwOptions AS DWORD, ptrRect AS IntPtr, ;
		pszData AS PSZ, dwLength AS DWORD) AS VOID STRICT

		RETURN
/// <exclude />
	[Obsolete];
	METHOD __EditDispatch(uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LOGIC STRICT
		// Not needed, handled by DotNet
		RETURN FALSE
/// <exclude />
	METHOD __EnableNotify() AS VOID STRICT
		IF iDeferNotifyCount>0
			--iDeferNotifyCount
		ENDIF
		IF iDeferNotifyCount==0 .AND. oDataServer!=NULL_OBJECT
			oDataServer:ResetNotification()
		ENDIF
		RETURN

 /// <exclude />
	METHOD __EnableSelection(kStyle AS INT) AS VOID STRICT

		// Turn off old mode
		IF !SELF:__IsValid
			RETURN
		ENDIF
		SWITCH kStyle
		CASE ssNoSelection
			__DataGridView:MultiSelect := FALSE
			__DataGridView:SelectionMode := DataGridViewSelectionMode.FullRowSelect
		CASE ssSingleSelection
			__DataGridView:MultiSelect := FALSE
			__DataGridView:SelectionMode := DataGridViewSelectionMode.FullRowSelect
		CASE ssExtendedSelection
			__DataGridView:MultiSelect := TRUE
			__DataGridView:SelectionMode := DataGridViewSelectionMode.CellSelect

		CASE ssBlockSelection
			__DataGridView:MultiSelect := TRUE
			__DataGridView:SelectionMode := DataGridViewSelectionMode.CellSelect
		END SWITCH

		RETURN

	METHOD __EndEditField(dwChar AS DWORD) AS VOID STRICT
		IF !SELF:__IsValid
			RETURN
		ENDIF
		IF (oCtrl != NULL .and. __DataGridView:IsCurrentCellInEditMode)
			SELF:__SaveEditChanges()
		ENDIF
		RETURN


 /// <exclude />
	METHOD __SaveEditChanges() AS VOID STRICT
		LOCAL oDCol AS DataColumn
		LOCAL c AS STRING
		LOCAL oCell AS DataGridViewCell
		LOCAL oRow  AS VODataGridViewRow
		LOCAL oCol  AS VODataGridViewColumn
		IF !SELF:__IsValid
			RETURN
		ENDIF
		IF SELF:__DataGridView:CurrentCell != NULL_OBJECT
			oCell := SELF:__DataGridView:CurrentCell
			oCol  := (VODataGridViewColumn) oCell:OwningColumn
			oDCol := oCol:DataColumn
			oRow  := (VODataGridViewRow) oCell:OwningRow


			IF oRow:RecNo== oDataServer:RecNo
				IF __DataGridView:IsCurrentCellInEditMode
					SELF:__DataGridView:EndEdit()
				ENDIF
				IF oCell:Value != NULL
					c := (STRING) oCell:Value
				ELSE
					c := ""
				ENDIF
				oDCol:SetValue(c:TrimEnd())
				SELF:ColumnFocusChange(oDCol, TRUE)
			ELSE
				SELF:__DataGridView:CancelEdit()
			ENDIF
		ENDIF

 /// <exclude />
	METHOD __FieldChange() AS VOID STRICT
		LOCAL iRecNo AS INT
		LOCAL row AS VODataGridViewRow

		IF oDataServer != NULL_OBJECT
			iRecNo := oDataServer:RecNo
			row  := SELF:__GetRowAtRecNo(iRecNo)
			IF row != NULL_OBJECT
				SELF:SuspendUpdate()
				SELF:__DeferNotify()
				SELF:__FillRecord(row)
				SELF:__EnableNotify()
				SELF:RestoreUpdate()
			ENDIF
		ENDIF

		RETURN

 /// <exclude />
	METHOD __FillRecord(oRow AS VODataGridViewRow ) AS VOID STRICT
		LOCAL iLen AS INT
		LOCAL i AS INT
		LOCAL oColumn AS DataColumn
		LOCAL oValues AS OBJECT[]
		SELF:__RefreshData()
		iLen := INT(ALen(aColumn))
		oValues := OBJECT[]{iLen}

		FOR i:=1 UPTO iLen
			oColumn	:= aColumn[i]
			oValues[i] := oColumn:GetValue()
		NEXT
		oRow:SetValues(oValues)
		RETURN

 /// <exclude />
	METHOD __FindColumn(uColumn AS USUAL) AS DWORD STRICT
		// returns Index into array
		LOCAL sIndex AS SYMBOL
		LOCAL dwType AS DWORD
		LOCAL dwI, dwCount AS DWORD

		dwType := UsualType(uColumn)
		SWITCH dwType
		CASE LONGINT
			IF uColumn > 0 .AND. uColumn <= ALen(aColumn)
				RETURN uColumn
			ENDIF
			RETURN 0
		CASE SYMBOL
		CASE STRING
			sIndex := IIF(dwType == SYMBOL, uColumn, String2Symbol(uColumn))
			dwCount := ALen(aColumn)
			FOR dwI := 1 UPTO dwCount
				VAR oDCol := (DataColumn) aColumn[dwI]
				IF oDCol:NameSym == sIndex
					RETURN dwI
				ENDIF
			NEXT //dwI
			RETURN 0
	  CASE VOID
			RETURN 0
	  OTHERWISE
			IF ((OBJECT) uColumn) IS DataColumn VAR oDCol
				dwCount := ALen(aColumn)
				FOR dwI := 1 UPTO dwCount
					IF oDCol = aColumn[dwI]
						RETURN dwI
					ENDIF
			   NEXT //dwI
			ENDIF
			WCError{#__FindColumn,#DataBrowser,__WCSTypeError,uColumn,1}:Throw()
		END SWITCH

		RETURN 0

 /// <exclude />
	METHOD __GatherColumns() AS VOID STRICT
		FOREACH oColumn AS DataColumn IN aColumn
			IF oColumn != NULL_OBJECT
				oColumn:__Gather()
			ENDIF
		NEXT

 /// <exclude />
	METHOD __GetColumn(nIndex AS INT) AS DataColumn STRICT
		// nIndex is the Zero based GridView Column Index
		IF SELF:__IsValid
			FOREACH oCol AS VODataGridViewColumn IN SELF:__DataGridView:Columns
				IF oCol:Index == nIndex
					RETURN oCol:DataColumn
				ENDIF
			NEXT
		ENDIF
		RETURN NULL_OBJECT


 /// <exclude />
	METHOD __GetRowAtRecNo(iRecNo AS INT) AS VODataGridViewRow STRICT
		IF oRowDict:ContainsKey(iRecNo)
			RETURN oRowDict[iRecNo]
		ENDIF
		RETURN NULL_OBJECT


 /// <exclude />
	METHOD __GetRecordNo(nRow AS INT) AS LONGINT STRICT
		IF nRow <= SELF:RowCount .and. nRow >0
			LOCAL oRow AS VODataGridViewRow
			oRow := (VODataGridViewRow) SELF:__DataGridView:Rows[nRow-1]
			RETURN oRow:RecNo
		ENDIF
		RETURN 0

 /// <exclude />
	METHOD __IsDataServerEmpty() AS LOGIC STRICT
		IF oDataServer != NULL_OBJECT
			RETURN oDataServer:BoF .AND. oDataServer:EoF
		ENDIF
		RETURN TRUE

 /// <exclude />
	METHOD __FocusToCurrentRow AS VOID STRICT
		LOCAL nCurrentRow AS LONG
		LOCAL nDisplayedRows AS LONG
		LOCAL nFirstRow AS LONG
		IF SELF:__HasColumns .and. SELF:RowCount > 0
			SELF:SuspendUpdate()
			nDisplayedRows	:= __DataGridView:VisibleRows
			nCurrentRow		:= __DataGridView:CurrentCell:RowIndex
			nFirstRow		:= __DataGridView:TopRowIndex
			IF nFirstRow+nDisplayedRows < nCurrentRow  .and. nFirstRow+nDisplayedRows>= 0
				__DataGridView:TopRowIndex := nCurrentRow - nFirstRow
			ENDIF
			SELF:RestoreUpdate()
		ENDIF
		RETURN
 /// <exclude />
	METHOD __IsFocusRecordInView() AS LOGIC STRICT
		// The Focused record is the record that matches the current record in the server
		LOCAL oRow AS VODataGridViewRow
		oRow := SELF:__GetRowAtRecNo(oDataServer:RecNo)
		RETURN oRow != NULL_OBJECT

	// Obsolete
	/// <exclude />
	ASSIGN __LastChar(dwNewLastChar AS DWORD)  STRICT
		//(dwLastChar := dwNewLastChar)


 /// <exclude />
	METHOD __NewFocusField(nField AS LONG ) AS VOID STRICT
		IF SELF:__HasColumns
			IF nFocusField != 0
				SELF:ColumnFocusChange(SELF:__GetColumn(nFocusField), FALSE)
			ENDIF

			IF nField != 0
				SELF:ColumnFocusChange(SELF:__GetColumn(nField), TRUE)
			ENDIF
		ENDIF
		nFocusField := nField
		RETURN

 /// <exclude />
	METHOD __NewFocusRecord(oRow AS VODataGridViewRow) AS VOID STRICT
		LOCAL iRecNo AS INT
		IF SELF:RowCount > 0
			iRecNo := oRow:RecNo
			IF iRecNo != 0
				IF oDataServer:RecNo == 0
					oDataServer:Update()
					IF oDataServer:RecNo != iRecNo
						oDataServer:GoTo(iRecNo)
					ENDIF
				ELSE
					iDeferNotifyCount := iDeferNotifyCount + 1
					IF oDataServer:RecNo != iRecNo
						oDataServer:GoTo(iRecNo)
					ENDIF
					iDeferNotifyCount := iDeferNotifyCount - 1
				ENDIF
			ENDIF
		ENDIF
		RETURN

 /// <exclude />
	METHOD __NotifyChanges(kNotify AS DWORD) AS USUAL STRICT
		IF iDeferNotifyCount == 0 .and. SELF:__IsValid
			// if in the middle of an edit - end it
			IF (SELF:__DataGridView:IsCurrentCellInEditMode)
				SELF:__EndEditField(0)
			ENDIF

			SWITCH kNotify
			//CASE .not. __DataGridView:Visible
			//	RETURN TRUE
			CASE GBNFY_INTENTTOMOVE
				RETURN SELF:Validate()
			CASE GBNFY_RECORDCHANGE
				SELF:__RecordChange()
			CASE GBNFY_DOGOTOP
			CASE GBNFY_DOGOEND
				SELF:__RecordChange()
				SELF:__RefreshBuffer()
			CASE GBNFY_FIELDCHANGE
				SELF:__FieldChange()
			CASE GBNFY_FILECHANGE
				SELF:__RefreshBuffer()
			CASE GBNFY_DONEWROW
				SELF:__RefreshBuffer()
			CASE GBNFY_DODELETE
				SELF:__RecordDelete()
			END SWITCH
			SELF:SetVScrollBar()
		ELSE
			IF kNotify == GBNFY_INTENTTOMOVE
				RETURN TRUE
			ELSE
				RETURN NIL
			ENDIF
		ENDIF
		RETURN NIL


 /// <exclude />
	METHOD __RecordChange() AS VOID STRICT
		// Move the selected row in the grid After a change in the record pointer of the DataServer
		LOCAL iRecNo AS INT
		LOCAL row AS VODataGridViewRow
		LOCAL nCell AS LONG
		IF ! SELF:__IsValid
			RETURN
		ENDIF
		IF ! __DataGridView:Visible
			RETURN
		ENDIF
		IF oDataServer != NULL_OBJECT .AND. !oDataServer:EoF
			iRecNo := oDataServer:RecNo
			IF iRecNo == 0
				oDataServer:SuspendNotification()
				oDataServer:GoTop()
				oDataServer:ResetNotification()
				iRecNo := oDataServer:RecNo
			ENDIF

			// check if record is already focused
			row := SELF:__CurrentRow
			IF __DataGridView:CurrentCell != NULL_OBJECT
				nCell := __DataGridView:CurrentCell:ColumnIndex
			ELSE
				nCell := 0
			ENDIF
			IF row != NULL_OBJECT .AND. row:RecNo == iRecNo
				// The right row is already selected
				//
				__DataGridView:CurrentCell := row:Cells[nCell]
			ELSE
				// check if record is in the buffer
				// If not then we refresh the buffer
				row := SELF:__GetRowAtRecNo(iRecNo)
				IF row == NULL_OBJECT
					SELF:__DeferNotify()
					SELF:__BuildBuffer()
					row := SELF:__GetRowAtRecNo(iRecNo)
					SELF:__EnableNotify()
				ENDIF
				// record is in the buffer so focus it
				IF row != NULL_OBJECT
					IF row:Cells[nCell]:Visible
						__DataGridView:CurrentCell := row:Cells[nCell]
					ENDIF

					IF !SELF:__IsFocusRecordInView()
						// Refresh buffer and load buffer that contains the current row from the server
						IF !lHasTop .and. !lHasBottom
							SELF:__RefreshBuffer()
						ELSE
							// This should not happen
							__DataGridView:CurrentCell := __DataGridView:CurrentCell
						ENDIF
					ENDIF
				ELSE
					// This should not happen
					__DataGridView:CurrentCell := __DataGridView:CurrentCell
				ENDIF
			ENDIF
		ENDIF
		SELF:VScrollBarShowWhenNeeded()
		RETURN

 /// <exclude />
	METHOD __RecordDelete() AS VOID STRICT
		LOCAL row AS VODataGridViewRow
		LOCAL iRecno AS INT
		LOCAL iTopRecno AS INT
		IF ! SELF:__IsValid
			RETURN
		ENDIF
		TRY
			iRecno := oDataServer:RecNo
			SELF:SuspendUpdate()
			SELF:__DeferNotify()

			row := SELF:__DataGridView:FirstRow
			IF row != NULL_OBJECT
				SELF:Skipper(1)
			ELSE
				iTopRecno := SELF:__GetRecordNo(1)
				IF iTopRecno != iRecno
					IF oDataServer:RecNo != iTopRecno
						oDataServer:GoTo(iTopRecno)
					ENDIF
				ELSE
					SELF:Skipper(1)
				ENDIF
			ENDIF

			SELF:__ClearBuffers()
			IF !SELF:__IsDataServerEmpty()
				SELF:__BuildNewBuffer(oDataServer:RecNo)
			ENDIF

			oDataServer:GoTo(iRecno)
		FINALLY
			SELF:__EnableNotify()
			SELF:RestoreUpdate()
		END TRY
		RETURN

 /// <exclude />
	METHOD __RefreshBuffer() AS VOID STRICT
		IF SELF:__HasColumns .and. SELF:lInEvent == 0
			SELF:SuspendUpdate()
			SELF:__BuildBuffer()
			SELF:RestoreUpdate()
		ENDIF
		RETURN

 /// <exclude />
	METHOD __RefreshData() AS VOID STRICT
		FOREACH oColumn AS DataColumn IN aColumn
			IF oColumn != NULL_OBJECT
				oColumn:__Scatter()
			ENDIF
		NEXT
		RETURN

 /// <exclude />
	METHOD __RefreshField(uFieldName AS USUAL) AS VOID
		LOCAL symFieldName AS SYMBOL
		LOCAL oDF AS DataField
		IF IsSymbol(uFieldName)
			symFieldName:=uFieldName
			FOREACH oCol AS DataColumn IN aColumn
				oDF  := oCol:__DataField
				IF (oDF != NULL_OBJECT) .AND. (oDF:NameSym == symFieldName)
					oCol:__Scatter()
				ENDIF
			NEXT
		ENDIF
		RETURN

 /// <exclude />
	METHOD __RegisterFieldLinks(oDS AS DataServer) AS LOGIC STRICT
		LOCAL iDF AS DWORD


		// Link in columns depending on two conditions.
		// If we already have columns registered with the browser then assume
		// auto layout is not desired.

		// If no columns have been registered with the form then we
		// assume auto layout is desired.

		IF aColumn:Length > 0
			FOREACH oDC AS DataColumn IN aColumn
				iDF := oDS:FieldPos(oDC:NameSym)
				IF iDF>0 .AND. IsNil(oDC:Server) // Only one datafield per column
					oDC:LinkDF(oDS, iDF) // Exit here, only one column per
					lLinked := TRUE
				ENDIF
			NEXT
		ELSE
			// We need to do an auto layout for the form
			lLinked := .T.
			SELF:__AutoLayout()
		ENDIF

		//// Register the form as a client of the Server
		IF lLinked
			oDS:RegisterClient(SELF)
		ENDIF

		RETURN lLinked


 /// <exclude />
	METHOD __StatusOK() AS DataColumn STRICT
		FOREACH oColumn AS DataColumn IN aColumn
			IF oColumn:Status != NIL
				RETURN oColumn
			ENDIF
		NEXT

		RETURN NULL_OBJECT

 /// <exclude />
	METHOD __TestForBottom() AS LOGIC STRICT
		LOCAL lRetCode AS LOGIC
		LOCAL iRecNo AS INT

		IF oDataServer:EoF
			RETURN TRUE
		ENDIF

		iRecNo := oDataServer:RecNo
		oDataServer:Skip(1)
		lRetCode := oDataServer:EoF
		IF oDataServer:RecNo != iRecNo
			oDataServer:GoTo(iRecNo)
		ENDIF

		RETURN lRetCode

 /// <exclude />
	METHOD __TestForTop() AS LOGIC STRICT
		LOCAL lRetCode AS LOGIC
		LOCAL iRecNo AS INT

		IF oDataServer:BoF
			RETURN TRUE
		ENDIF
		iRecNo := oDataServer:RecNo

		oDataServer:Skip(-1)
		lRetCode := oDataServer:BoF
		IF oDataServer:RecNo != iRecNo
			oDataServer:GoTo(iRecNo)
		ENDIF

		RETURN lRetCode

	//RH Performance fix
 /// <exclude />
	METHOD __UnLinkColumns() AS VOID STRICT
		IF aColumn != NULL
			FOREACH oColumn AS DataColumn IN aColumn
				IF oColumn != NULL_OBJECT
					oColumn:__UnLink(oDataServer)
				ENDIF
			NEXT
		ENDIF
		RETURN

	METHOD __Unlink(oDS := NIL AS USUAL) AS VOSDK.Control STRICT
		SELF:__UnLinkColumns()
		IF oDataServer != NULL_OBJECT
			oDataServer:UnRegisterClient(SELF)
			oDataServer := NULL_OBJECT
		ENDIF
		RETURN SELF


/// <include file="Gui.xml" path="doc/DataBrowser.AddColumn/*" />
	METHOD AddColumn(oGColumn, nIndex)
		LOCAL i AS INT
		LOCAL iLen AS INT
		LOCAL dwPosition AS DWORD
		LOCAL lRetCode AS LOGIC

		SELF:SuspendUpdate()

		IF !IsLong(nIndex) .OR. (nIndex == 0) .OR. (nIndex > ALen(aColumn))
			dwPosition := 0 // append to tail
		ELSE
			dwPosition := SELF:__FindColumn(nIndex)
		ENDIF

		// add array of columns if parameter is array
		IF IsArray(oGColumn)
			iLen := INT(ALen(oGColumn))
			FOR i := 1 UPTO iLen
				lRetCode := SELF:__AddColumn(oGColumn[i], INT(dwPosition))
				IF !lRetCode
					EXIT
				ENDIF
				// iPosition++
			NEXT
		ELSE
			lRetCode := SELF:__AddColumn(oGColumn, INT(dwPosition))
		ENDIF

		// if data server connection - refresh columns
		IF (oDataServer != NULL_OBJECT)

			SELF:__RefreshBuffer()
		ENDIF

		SELF:RestoreUpdate()

		RETURN lRetCode

	METHOD AsString as string strict
		RETURN ResourceString{__WCSDBrowserObject}:Value

/// <include file="Gui.xml" path="doc/DataBrowser.Background/*" />
	ACCESS Background  AS Brush
		RETURN SELF:__DataGridView:BackColor

/// <include file="Gui.xml" path="doc/DataBrowser.Background/*" />
	ASSIGN Background(oBrush AS Brush)
		SELF:ChangeBackground(oBrush, gblText)
		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.CanUndo/*" />
	METHOD CanUndo()
		IF SELF:CellEdit != NULL_OBJECT
			RETURN SELF:CellEdit:CanUndo
		ENDIF

		RETURN FALSE

/// <include file="Gui.xml" path="doc/DataBrowser.Caption/*" />
	ASSIGN Caption(sCaption as STRING)
		SELF:SetCaption(sCaption)
		RETURN

	METHOD CellCaptionDoubleClick( oColumn ) CLIPPER
/// <include file="Gui.xml" path="doc/DataBrowser.CellEdit/*" />
		RETURN NIL



	METHOD CellDoubleClick() CLIPPER
		IF SELF:__IsValid .and. SELF:__DataGridView:CurrentCell != NULL_OBJECT
			IF !SELF:__DataGridView:Readonly .and. ! SELF:__DataGridView:CurrentCell:ReadOnly
				SELF:__DataGridView:BeginEdit(TRUE)
			ENDIF
		ENDIF
		RETURN NIL
/// <include file="Gui.xml" path="doc/DataBrowser.CellEdit/*" />

	ACCESS CellEdit AS System.Windows.Forms.TextBox
		LOCAL oControl AS System.Windows.Forms.Control
		IF SELF:__IsValid
			oControl := __DataGridView:EditingControl
			IF oControl:GetType() == typeof(System.Windows.Forms.TextBox)
				RETURN (System.Windows.Forms.TextBox) oControl
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/DataBrowser.ChangeBackground/*" />
	METHOD ChangeBackground ( oBrush AS USUAL, kWhere AS INT )
		// Todo ChangeBackground
		LOCAL oNewBrush AS VOSDK.Brush
		IF ! SELF:__IsValid
			RETURN SELF
		ENDIF
		IF !(oBrush IS Brush)
			WCError{#ChangeBackground,#DataBrowser,__WCSTypeError,oBrush,1}:Throw()
		ELSE
			oNewBrush := oBrush
		ENDIF

		SWITCH kWhere
		CASE gblCaption
			// Not supported
			NOP
		CASE gblText
			SELF:__DataGridView:DefaultCellStyle:BackColor := oNewBrush
			SELF:__DataGridView:BackGroundColor := oNewBrush
		CASE gblColCaption
			SELF:__DataGridView:ColumnHeadersDefaultCellStyle:BackColor := oNewBrush

		CASE gblButton
			// Not supported
			NOP
		CASE gblColButton
			// Not supported
			NOP
		CASE gblHiText
			SELF:__DataGridView:DefaultCellStyle:SelectionBackColor := oNewBrush

		END SWITCH

		RETURN SELF

/// <include file="Gui.xml" path="doc/DataBrowser.ChangeFont/*" />
	METHOD ChangeFont(oFont AS Font, kWhere AS INT)
		LOCAL oNewFont AS VOSDK.Font

		IF ! SELF:__IsValid
			RETURN SELF
		ENDIF

		oNewFont := oFont

		SWITCH (INT) kWhere
		CASE gblCaption
			// Not supported
			NOP
		CASE gblText
			SELF:__DataGridView:DefaultCellStyle:Font := oNewFont

		CASE gblColCaption
			SELF:__DataGridView:ColumnHeadersDefaultCellStyle:Font := oNewFont

		CASE gblButton
		CASE gblColButton
		CASE gblHiText
			SELF:__DataGridView:Font:= oNewFont

		END SWITCH

		RETURN SELF

	METHOD ChangeTextColor(oColor AS USUAL , kWhere AS INT)
		LOCAL oNewColor AS VOSDK.Color
		LOCAL oOldColor AS VOSDK.Color
		IF ! SELF:__IsValid
			RETURN SELF
		ENDIF

		IF IsNumeric(oColor)
			oNewColor := VOSDK.Color{oColor}
		ELSEIF !(oColor IS Color)
			WCError{#ChangeTextColor,#DataBrowser,__WCSTypeError,oColor,1}:Throw()
		ELSE
			oNewColor := oColor
		ENDIF


		SWITCH kWhere
		CASE gblCaption
			// Not supported
			oOldColor := SELF:__DataGridView:ForeColor
		CASE gblColCaption
			oOldColor := SELF:__DataGridView:ColumnHeadersDefaultCellStyle:ForeColor
			SELF:__DataGridView:ColumnHeadersDefaultCellStyle:ForeColor:= oNewColor

		CASE gblText
			oOldColor := SELF:__DataGridView:DefaultCellStyle:ForeColor
			SELF:__DataGridView:DefaultCellStyle:ForeColor:= oNewColor

		CASE gblButton
			// Not supported
			oOldColor := SELF:__DataGridView:ForeColor
		CASE gblColButton
			// Not supported
			oOldColor := SELF:__DataGridView:ForeColor
		CASE gblHiText
			oOldColor := SELF:__DataGridView:DefaultCellStyle:SelectionForeColor
			SELF:__DataGridView:DefaultCellStyle:SelectionForeColor := oNewColor
		END SWITCH


		RETURN oOldColor

/// <include file="Gui.xml" path="doc/DataBrowser.Clear/*" />
	METHOD Clear()
		IF SELF:CellEdit != NULL_OBJECT
			SELF:CellEdit:Clear()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/DataBrowser.ColPos/*" />
	METHOD ColPos()
		LOCAL i, iLen AS INT
		LOCAL oDC AS DataColumn

		oDC := SELF:CurrentColumn
		IF oDC==NULL_OBJECT
			RETURN 0
		ENDIF

		iLen := INT(ALen(aColumn))
		FOR i:=1 UPTO iLen
			IF aColumn[i] == oDC
				RETURN i
			ENDIF
		NEXT

		RETURN 0

/// <include file="Gui.xml" path="doc/DataBrowser.ColumnCount/*" />
	ACCESS ColumnCount AS DWORD
		RETURN ALen(aColumn)

/// <include file="Gui.xml" path="doc/DataBrowser.ColumnFocusChange/*" />
	method ColumnFocusChange(oDataColumn as DataColumn, lHasFocus as logic) as object
        local oHL as HyperLabel
		if oDataColumn != null_object
            local oWin := (Window) self:Owner as Window
			oHL := oDataColumn:Status
			IF (oHL != NULL_OBJECT)
				oWin:@@StatusMessage(oHL:Description, MESSAGEERROR)
            elseif lHasFocus .and. (oDataColumn:HyperLabel != null_object)
                oHL := oDataColumn:HyperLabel
				oWin:@@StatusMessage(oHL:Description, MESSAGECONTROL)
			ENDIF
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/DataBrowser.ColumnMoved/*" />
	METHOD ColumnMoved(oColumn as DataColumn) AS VOID
		SELF:__EndEditField(0)
		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.ColumnReSize/*" />
	METHOD ColumnReSize(oColumn as DataColumn) AS VOID
		SELF:__EndEditField(0)
		RETURN

	ACCESS Columns AS ARRAY
		RETURN aColumn

/// <include file="Gui.xml" path="doc/DataBrowser.Copy/*" />
	METHOD Copy()   AS VOID STRICT
		IF SELF:CellEdit != NULL_OBJECT
			SELF:CellEdit:Copy()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.CurrentColumn/*" />
	ACCESS CurrentColumn AS DataColumn
		IF SELF:__IsValid .and. __DataGridView:CurrentCell != NULL_OBJECT
			RETURN SELF:__GetColumn(__DataGridView:CurrentCell:ColumnIndex)
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/DataBrowser.Cut/*" />
	METHOD Cut()   AS VOID STRICT
		IF SELF:CellEdit != NULL_OBJECT
			SELF:CellEdit:Cut()
		ENDIF

		RETURN

	METHOD Disable() AS VOID STRICT
		SUPER:Disable()

/// <include file="Gui.xml" path="doc/DataBrowser.Default/*" />
	METHOD Default(oEvent AS Event) AS VOID
		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.Destroy/*" />
	METHOD Destroy() AS USUAL CLIPPER
		SELF:__EndEditField(0)
		IF oCtrl != NULL_OBJECT
			__DataGridView:RowEnter -=  OnRowEnter
			__DataGridView:CellEndEdit -= OnCellEndEdit
			__DataGridView:SortCompare -= OnSortCompare
		ENDIF
		SELF:__Unlink()
        if self:aColumn != null
		    foreach oCol as DataColumn in self:aColumn
			    oCol:Destroy()
		    next
        endif

		nFocusField   := 0
		oDataServer := NULL_OBJECT
		//aColumn := NULL_ARRAY

		oTextPointer := NULL_OBJECT

		SUPER:Destroy()

		RETURN SELF


/// <include file="Gui.xml" path="doc/DataBrowser.Dispatch/*" />
	//METHOD Dispatch(oEvent)
	//	RETURN SUPER:Dispatch(oEvent)

/// <include file="Gui.xml" path="doc/DataBrowser.EditFont/*" />
	ACCESS EditFont
		RETURN NULL_OBJECT



/// <include file="Gui.xml" path="doc/DataBrowser.EnableBorder/*" />
	METHOD EnableBorder(kBorderType AS INT)
		// Todo Check if the border looks as expected
		IF ! SELF:__IsValid
			RETURN SELF
		ENDIF

		SWITCH kBorderType
		CASE BTSIZINGBORDER
			__DataGridView:BorderStyle := BorderStyle.Fixed3D
		CASE BTNONSIZINGBORDER
			__DataGridView:BorderStyle := BorderStyle.FixedSingle
		CASE BTNOBORDER
			__DataGridView:BorderStyle := BorderStyle.None
		OTHERWISE
			__DataGridView:BorderStyle := BorderStyle.Fixed3D
		END SWITCH

		RETURN NIL


/// <include file="Gui.xml" path="doc/DataBrowser.EnableColumnMove/*" />
	METHOD EnableColumnMove(lAllowMove:= TRUE AS LOGIC)  AS LOGIC

		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF

		SELF:__DataGridView:AllowUserToOrderColumns := lAllowMove

		RETURN TRUE


/// <include file="Gui.xml" path="doc/DataBrowser.EnableColumnReSize/*" />
	METHOD EnableColumnReSize(lAllowResize:= TRUE AS LOGIC)  AS LOGIC
		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF

		SELF:__DataGridView:AllowUserToResizeColumns := lAllowResize

		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataBrowser.EnableColumnTitles/*" />
	METHOD EnableColumnTitles(lEnable := TRUE AS LOGIC)  AS LOGIC

		IF lColumnTitles == lEnable
			RETURN TRUE
		ENDIF
		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF

		SELF:SuspendUpdate()

		lColumnTitles := lEnable

		SELF:__DataGridView:ColumnHeadersVisible := lColumnTitles
		SELF:RestoreUpdate()

		RETURN TRUE


/// <include file="Gui.xml" path="doc/DataBrowser.EnableGrid/*" />
	METHOD EnableGrid ( lShowGrid := TRUE AS LOGIC)  AS LOGIC
		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF
		IF lShowGrid
			SELF:__DataGridView:CellBorderStyle := DataGridViewCellBorderStyle.Single
		ELSE
			SELF:__DataGridView:CellBorderStyle := DataGridViewCellBorderStyle.None
		ENDIF
		RETURN TRUE


/// <include file="Gui.xml" path="doc/DataBrowser.EnableHorizontalScroll/*" />
	METHOD EnableHorizontalScroll ( lAllowScroll := TRUE AS LOGIC)  AS LOGIC
		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF

		IF lAllowScroll
			__DataGridView:ScrollBars := ScrollBars.Horizontal
		ELSE
			__DataGridView:ScrollBars := ScrollBars.None
		ENDIF
		RETURN TRUE


/// <include file="Gui.xml" path="doc/DataBrowser.EnableHorizontalSplit/*" />
	METHOD EnableHorizontalSplit ( lShowSplit := TRUE AS LOGIC)  AS LOGIC
		//Riz This was never implemented
		RETURN FALSE

/// <include file="Gui.xml" path="doc/DataBrowser.EnableVerticalScroll/*" />
	METHOD EnableVerticalScroll ( lAllowScroll := TRUE AS LOGIC)  AS LOGIC

		IF lAllowScroll
			SELF:VScrollBar:Visible := TRUE
		ELSE
			SELF:VScrollBar:Visible := FALSE
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataBrowser.EnableVerticalSplit/*" />
	METHOD EnableVerticalSplit(lShowSplit, nMode)
		// Todo: Enable vertical split, using Frozen Columns ?

		/*	IF !IsNil(lShowSplit)
		IF !IsLogic(lShowSplit)
		WCError{#EnableVerticalSplit,#DataBrowser,__WCSTypeError,lShowSplit,1}:Throw()
		ENDIF
		ELSE
		lShowSplit := TRUE
		ENDIF

		IF !IsNil(nMode)
		IF !IsLong(nMode)
		WCError{#EnableVerticalSplit,#DataBrowser,__WCSTypeError,nMode,2}:Throw()
		ENDIF
		ELSE
		nMode := GBSSBMIDDLE
		ENDIF

		IF lShowSplit
		CntStyleSet( hWnd, CTS_SPLITBAR)
		CntSpltBarCreate( hWnd, nMode, 0)
		ELSE
		CntSpltBarDelete( hWnd, 0, 0)
		CntStyleClear( hWnd, CTS_SPLITBAR)
		ENDIF
		*/
		RETURN SELF

/// <include file="Gui.xml" path="doc/DataBrowser.Error/*" />
	METHOD Error(oErrorObj)


		RETURN SELF


/// <include file="Gui.xml" path="doc/DataBrowser.Font/*" />
	ACCESS Font  AS Font
		RETURN SELF:__DataGridView:Font

/// <include file="Gui.xml" path="doc/DataBrowser.Font/*" />
	ASSIGN Font(oFont AS Font)
		SELF:ChangeFont(oFont, gblText)
		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.GetColumn/*" />
	METHOD GetColumn(xColumnID)
		LOCAL dwPosition AS DWORD

		IF !IsLong(xColumnID) .AND. !IsSymbol(xColumnID) .AND. !IsString(xColumnID)
			WCError{#GetColumn,#DataBrowser,__WCSTypeError,xColumnID,1}:Throw()
		ENDIF

		dwPosition := SELF:__FindColumn(xColumnID)
		IF dwPosition != 0
			RETURN aColumn[dwPosition]
		ENDIF

		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/DataBrowser.HiBackground/*" />
	ACCESS HiBackground AS Brush
		IF ! SELF:__IsValid
			RETURN NULL_OBJECT
		ENDIF

		RETURN SELF:__DataGridView:DefaultCellStyle:BackColor

	ACCESS HyperLabel AS HyperLabel
		RETURN SUPER:HyperLabel

	ASSIGN HyperLabel (oHL AS HyperLabel)
		SUPER:HyperLabel := oHL
		IF oHL != NULL_OBJECT .and. SELF:__IsValid
			SELF:__DataGridView:Text := "Browser:"+oHL:Name
		ENDIF

/// <include file="Gui.xml" path="doc/DataBrowser.ctor/*" />
	constructor(oOwner := null as Window, xID:= 1000 as long, oPoint:= null as Point, oDimension := null as Dimension)
		LOCAL oBB AS BoundingBox
		LOCAL oWin AS Window
		local nHeight as long
		if oOwner == null
			WCError{#Init,#DataBrowser,__WCSTypeError,oOwner,1}:Throw()
		ENDIF
		oWin := oOwner
		oRowDict := System.Collections.Generic.Dictionary<int, VODataGridViewRow>{}

		IF oOwner IS DataWindow VAR oDW
			oBB			:= oDW:CanvasArea
			IF oWin:ToolBar != NULL_OBJECT
                nHeight := oWin:ToolBar:Size:Height
            else
                nHeight := 0
			ENDIF
			oPoint		:= VOSDK.Point{0,nHeight}
			oDimension	:= Dimension{oBB:Width,oBB:Height-nHeight}
		ENDIF


		SUPER(oWin, xID, oPoint, oDimension, CONTAINER_CLASS)

		iBufferGranularity := INT(_CAST, QueryRTRegInt("Browser", "Granularity"))
		IF (iBufferGranularity == 0)
			iBufferGranularity := 64
		ENDIF
		iBufferGranularity := Max(16, iBufferGranularity)

		iBufferMaximum := INT(_CAST, QueryRTRegInt("Browser", "Maximum"))
		IF (iBufferMaximum == 0)
			iBufferMaximum := 1024
		ENDIF
		iBufferMaximum := Max(2 * iBufferGranularity, iBufferMaximum)

		lColumnTitles := TRUE
		lUseDefCaption := TRUE
		lUseDefColCaption := TRUE
		lUseDefText := TRUE
		lUseDefButton := TRUE
		lUseDefColButton := TRUE
		lUseDefHiText := TRUE
		aColumn := {}

		SELF:SetFocus() // Force Handle to be created


		lUse3dLook := TRUE
		// This appears to affect over all container

		//CntColorSet( hWnd, CNTCOLOR_3DHIGH,		GuiWin32.GetSysColor(COLOR_BTNHIGHLIGHT))
		//CntColorSet( hWnd, CNTCOLOR_3DSHADOW,	GuiWin32.GetSysColor(COLOR_BTNSHADOW))
		IF SELF:__IsValid
			SELF:__DataGridView:DefaultCellStyle:SelectionBackColor := System.Drawing.SystemColors.Highlight
			SELF:__DataGridView:DefaultCellStyle:SelectionForeColor := System.Drawing.SystemColors.HighlightText


			////Set title defaults for 3D appearance
			//CntColorSet( hWnd, CNTCOLOR_TITLE, GuiWin32.GetSysColor(COLOR_BTNTEXT))
			SELF:__DataGridView:ColumnHeadersBorderStyle := DataGridViewHeaderBorderStyle.Raised
			SELF:__DataGridView:ColumnHeadersHeightSizeMode := DataGridViewColumnHeadersHeightSizeMode.AutoSize

			SELF:__DataGridView:ColumnHeadersDefaultCellStyle:BackColor	:= System.Drawing.SystemColors.ButtonFace
			SELF:__DataGridView:ColumnHeadersDefaultCellStyle:ForeColor := System.Drawing.SystemColors.ButtonHighlight
			SELF:__DataGridView:BackColor := System.Drawing.SystemColors.Window
			SELF:__DataGridView:BackGroundColor := System.Drawing.SystemColors.Window
			SELF:__DataGridView:CellBorderStyle := DataGridViewCellBorderStyle.Single
			//CntColorSet( hWnd, CNTCOLOR_TTLBKGD, GuiWin32.GetSysColor(COLOR_BTNFACE))

			//// Colors for buttons
			//CntColorSet( hWnd, CNTCOLOR_TTLBTNTXT, GuiWin32.GetSysColor(COLOR_BTNTEXT))
			//CntColorSet( hWnd, CNTCOLOR_TTLBTNBKGD, GuiWin32.GetSysColor(COLOR_BTNFACE))
			//CntColorSet( hWnd, CNTCOLOR_FLDBTNTXT, GuiWin32.GetSysColor(COLOR_BTNTEXT))
			//CntColorSet( hWnd, CNTCOLOR_FLDBTNBKGD, GuiWin32.GetSysColor(COLOR_BTNFACE))

			//// Default line spacing - Quarter Line
			//CntRowHtSet( hWnd, 1, CA_LS_MEDIUM)


			// Set default font
			//CntFontSet( hWnd, GuiWin32.GetStockObject(DEFAULT_GUI_FONT), CF_GENERAL)

			//CntAttribSet( hWnd, CA_APPSPLITABLE)
			//CntRangeExSet( hWnd, 0, 0)

			SELF:__EnableSelection(ssSingleSelection)

			SELF:EnableColumnMove(TRUE)
			SELF:EnableColumnReSize(TRUE)
			SELF:EnableGrid(TRUE)
			SELF:EnableHorizontalScroll(TRUE)
			SELF:EnableVerticalScroll(TRUE)

			//IF (IsInstanceOfUsual(oOwner, #DataWindow))
			//	SELF:__AutoResize()
			//ENDIF
		ENDIF

		RETURN

	METHOD IsCellReadOnly() AS LOGIC
		LOCAL oColumn AS DataColumn
		IF SELF:__IsValid .and. SELF:__DataGridView:ReadOnly
			RETURN TRUE
		ENDIF
		oColumn := SELF:CurrentColumn
		IF oColumn != NULL_OBJECT
			RETURN oColumn:__Column:ReadOnly
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataBrowser.NewRow/*" />
	METHOD NewRow()
		RETURN FALSE

/// <include file="Gui.xml" path="doc/DataBrowser.Notify/*" />
	METHOD Notify(kNotification, uDescription)

		SWITCH (INT) kNotification
		CASE NOTIFYCOMPLETION
			SELF:__NotifyChanges(GBNFY_COMPLETION)
			nOldRecordNum := oDataServer:RecNo

		CASE NOTIFYINTENTTOMOVE
			RETURN SELF:__NotifyChanges(GBNFY_INTENTTOMOVE)
		CASE NOTIFYFILECHANGE
			SELF:__RefreshData()
			SELF:__NotifyChanges(GBNFY_FILECHANGE)
			SELF:__RefreshData()
			nOldRecordNum := oDataServer:RecNo
		CASE NOTIFYFIELDCHANGE
			SELF:__RefreshField(uDescription)
			SELF:__NotifyChanges(GBNFY_FIELDCHANGE)
		CASE NOTIFYCLOSE
			SELF:__Unlink()
		CASE NOTIFYRECORDCHANGE
			SELF:__RefreshData()
			IF nOldRecordNum != oDataServer:RecNo
				SELF:__NotifyChanges(GBNFY_RECORDCHANGE)
				SELF:__RefreshData()
			ELSE
				SELF:__NotifyChanges(GBNFY_FIELDCHANGE)
			ENDIF
			nOldRecordNum := oDataServer:RecNo
		CASE NOTIFYGOBOTTOM
			SELF:__RefreshData()
			SELF:__NotifyChanges(GBNFY_DOGOEND)
			//ASend(aColumn, #__Scatter)
			SELF:__RefreshData()
			nOldRecordNum := oDataServer:RecNo
		CASE NOTIFYGOTOP
			SELF:__RefreshData()
			SELF:__NotifyChanges(GBNFY_DOGOTOP)
			SELF:__RefreshData()
			nOldRecordNum := oDataServer:RecNo
		CASE NOTIFYDELETE
			SELF:__RefreshData()
			SELF:__NotifyChanges(GBNFY_DODELETE)
			SELF:__RefreshData()
			nOldRecordNum := oDataServer:RecNo
		CASE NOTIFYAPPEND
			SELF:__RefreshData()
			SELF:__NotifyChanges(GBNFY_DONEWROW)
			SELF:__RefreshData()
			SELF:__ValidateColumns()
			nOldRecordNum := oDataServer:RecNo
		END SWITCH

		RETURN NIL
    /// <exclude />
	METHOD __ValidateColumns() AS VOID STRICT
		FOREACH oColumn AS DataColumn IN aColumn
			IF oColumn != NULL_OBJECT
				oColumn:PerformValidations()
			ENDIF
		NEXT

   /// <exclude />
	METHOD __ValidateFocusColNo(nCol AS LONG) AS LONG
		IF nCol >= 0 .and. nCol < __DataGridView:Columns:Count .and. __DataGridView:Columns[nCol]:Visible
			RETURN nCol
		ENDIF
		nCol := 0
		DO WHILE nCol < __DataGridView:Columns:COunt .and. !__DataGridView:Columns[nCol]:Visible
			nCol++
		ENDDO
		IF nCol >= __DataGridView:Columns:Count
			nCol := -1
		ENDIF
		RETURN nCol


/// <include file="Gui.xml" path="doc/DataBrowser.Paste/*" />
	METHOD Paste ( )   AS VOID STRICT
		IF SELF:CellEdit != NULL_OBJECT
			SELF:CellEdit:Paste()
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/DataBrowser.Pointer/*" />
	ACCESS Pointer AS Pointer
		RETURN oTextPointer

/// <include file="Gui.xml" path="doc/DataBrowser.Pointer/*" />
	ASSIGN Pointer(oPointer AS Pointer)
		SELF:SetPointer(oPointer, gblText)
		RETURN


/// <include file="Gui.xml" path="doc/DataBrowser.Refresh/*" />
	METHOD Refresh() AS VOID STRICT
		IF oDataServer!=NULL_OBJECT
			oDataServer:GoTo(oDataServer:RecNo) //Forces refresh if DBF was empty
		ENDIF
		SELF:__RefreshBuffer()
		SELF:__RefreshData()
		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.RemoveColumn/*" />
	METHOD RemoveColumn(uColumnOrIndex AS USUAL) AS DataColumn
		LOCAL oDC AS DataColumn
		LOCAL i AS DWORD

		i := SELF:__FindColumn(uColumnOrIndex)

		IF (i != 0)
			SELF:SuspendUpdate()
			IF oDataServer!=NULL_OBJECT
				SELF:__ClearBuffers()
			ENDIF

			oDC := aColumn[i]
			ATruedel(aColumn,i)
			IF SELF:__IsValid
				SELF:__DataGridView:Columns:Remove(oDC:oDataGridColumn)
			ENDIF
			oDC:__Owner := NULL_OBJECT

			// if data server connection - refresh columns
			IF oDataServer!=NULL_OBJECT
				SELF:__RefreshBuffer()
			ENDIF

			SELF:RestoreUpdate()
			IF SELF:__IsValid .and. SELF:__DataGridView:CurrentCell != NULL_OBJECT
				nFocusField := __DataGridView:CurrentCell:ColumnIndex+1
			ENDIF
		ENDIF

		RETURN oDC


/// <include file="Gui.xml" path="doc/DataBrowser.RestoreUpdate/*" />
	METHOD RestoreUpdate() AS VOID STRICT
		IF iDeferPaintCount!=0
			--iDeferPaintCount
		ENDIF

		IF iDeferPaintCount == 0 .and. SELF:__IsValid
			SELF:__DataGridView:ResumeLayout()
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/DataBrowser.RowCount/*" />
	ACCESS RowCount  AS LONG
		IF SELF:__IsValid
			RETURN SELF:__DataGridView:Rows:Count
		ENDIF
		RETURN 0

	ACCESS Server AS DataServer
		RETURN SELF:oDataServer

/// <include file="Gui.xml" path="doc/DataBrowser.SetCaption/*" />
	METHOD SetCaption(cText AS STRING) AS LOGIC
		IF SELF:__IsValid
			SELF:SuspendUpdate()
			SELF:__DataGridView:Text := cText
			SELF:RestoreUpdate()
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataBrowser.SetColumn/*" />
	METHOD SetColumn(oDataColumn AS DataColumn, nColumnNumber AS LONG)  AS DataColumn
		LOCAL oDC AS DataColumn
		oDC := SELF:GetColumn(nColumnNumber)
		IF oDC!=NULL_OBJECT
			SELF:SuspendUpdate()
			SELF:RemoveColumn(oDC)
			SELF:AddColumn(oDataColumn, nColumnNumber)
			SELF:RestoreUpdate()
		ENDIF
		RETURN oDC

/// <include file="Gui.xml" path="doc/DataBrowser.SetColumnFocus/*" />
	METHOD SetColumnFocus(oColumn AS DataColumn) AS LOGIC
		LOCAL oDC	AS DataColumn
		LOCAL iRow	:= -1 AS INT
		IF SELF:__IsValid .and. SELF:__DataGridView:CurrentCell != NULL_OBJECT
			iRow := SELF:__DataGridView:CurrentCell:RowIndex
		ENDIF
		oDC := oColumn
		IF SELF:__IsValid .AND. iRow >= 0 .AND. iRow < SELF:__DataGridView:Rows:Count
			IF oDC:oDataGridColumn:Visible
				SELF:__DataGridView:CurrentCell := SELF:__DataGridView:Rows[iRow]:Cells[oDC:oDataGridColumn:Index]
			ENDIF
		ENDIF
		RETURN oDC != NULL_OBJECT

/// <include file="Gui.xml" path="doc/DataBrowser.SetPointer/*" />
	METHOD SetPointer(oPointer, kWhere)
		//Todo
		//LOCAL iLoc AS DWORD

		//DEFAULT( REF oPointer, Pointer{PointerArrow})

		//IF !IsInstanceOfUsual(oPointer,#Pointer)
		//	WCError{#SetPointer,#Pointer,__WCSTypeError,oPointer,1}:Throw()
		//ENDIF

		//IF !IsNil(kWhere)
		//	IF !IsLong(kWhere)
		//		WCError{#SetPointer,#DataBrowser,__WCSTypeError,kWhere,2}:Throw()
		//	ENDIF
		//ENDIF

		//iLoc := CC_GENERAL
		//DO CASE
		//CASE (kWhere == GBLCAPTION)
		//	iLoc := CC_TITLE

		//CASE (kWhere == GBLCOLCAPTION)
		//	iLoc := CC_FLDTITLE

		//CASE (kWhere == GBLTEXT)
		//	iLoc := CC_GENERAL
		//	oTextPointer := oPointer
		//ENDCASE

		//CntCursorSet( hWnd, oPointer:Handle(), iLoc)

		RETURN NIL

/// <include file="Gui.xml" path="doc/DataBrowser.SetStandardStyle/*" />
	METHOD SetStandardStyle(kStyle := gbsControl3d AS LONG)  AS LOGIC
		IF ! SELF:__IsValid
			RETURN FALSE
		ENDIF
		SELF:SuspendUpdate()


		SWITCH kStyle
		CASE GBSREADONLY
			SELF:__DataGridView:ReadOnly := TRUE

		CASE GBSEDIT
			SELF:__DataGridView:ReadOnly := FALSE
		CASE GBSCONTROL3D
			// Not implemented
			NOP
		CASE GBSCONTROL2D
			// Not implemented
			NOP
		END SWITCH

		SELF:RestoreUpdate()

		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataBrowser.Show/*" />
	METHOD Show() AS VOID CLIPPER
		IF oDataServer != NULL_OBJECT
			IF !lIsShown
				SELF:__BuildBuffer()
			ENDIF
		ENDIF
		lIsShown := TRUE

		SUPER:Show()
		SELF:SetFocus()

		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.SuspendUpdate/*" />
	METHOD SuspendUpdate()   AS VOID STRICT

		IF iDeferPaintCount == 0 .and. SELF:__IsValid
			SELF:__DataGridView:SuspendLayout()
		ENDIF
		iDeferPaintCount := iDeferPaintCount + 1

		RETURN

/// <include file="Gui.xml" path="doc/DataBrowser.TextColor/*" />
	ACCESS TextColor AS Color
		IF SELF:__IsValid
			RETURN SELF:__DataGridView:DefaultCellStyle:ForeColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/DataBrowser.TextColor/*" />
	ASSIGN TextColor(oColor AS Color)
		SELF:ChangeTextColor(oColor, gblText)
		RETURN


/// <include file="Gui.xml" path="doc/DataBrowser.Undo/*" />
   METHOD Undo() AS VOID
		IF SELF:CellEdit != NULL_OBJECT
			SELF:CellEdit:Undo()
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/DataBrowser.Use/*" />
	METHOD Use(oServer AS DataServer) AS LOGIC
		IF oServer != NULL
			IF oDataServer != oServer
				IF oDataServer != NULL_OBJECT
					SELF:__Unlink()
				ENDIF
				oDataServer := oServer
				GuiWin32.SetWindowText(__DataGridView:Handle, "DataBrowser "+oDataServer:Name)
				SELF:__RegisterFieldLinks(oServer)
				SELF:__RefreshData()
				IF lLinked
					SELF:SuspendUpdate()
					SELF:__ClearBuffers()
					IF lIsShown
						SELF:__BuildBuffer()
					ENDIF
					SELF:__NewFocusField(0)

					SELF:RestoreUpdate()
					nOldRecordNum := oServer:RecNo
				ENDIF
			ENDIF
		ELSE
			SELF:__Unlink()
		ENDIF

		RETURN lLinked


/// <include file="Gui.xml" path="doc/DataBrowser.Validate/*" />
	METHOD Validate() AS LOGIC STRICT

		IF SELF:Owner IS DataWindow VAR oDW
			RETURN oDW:__CheckRecordStatus()
		ENDIF
		RETURN FALSE

	METHOD OnCellPainting(Sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellPaintingEventArgs) AS VOID
		IF (e:RowIndex == -1 .and. e:ColumnIndex > -1)
			e:PaintBackground(e:CellBounds, TRUE)
			SELF:RenderColumnHeader(e:Graphics, e:CellBounds, e:CellStyle:BackColor)
			SELF:RenderColumnHeaderBorder(e:Graphics, e:CellBounds, e:ColumnIndex)
			LOCAL IMPLIED brush := System.Drawing.SolidBrush{e:CellStyle:ForeColor}
			LOCAL IMPLIED sf := @@StringFormat{}
			sf:LineAlignment := StringAlignment.Center
			sf:Alignment := System.Drawing.StringAlignment.Center
			e:Graphics:DrawString(e:Value:ToString(), e:CellStyle:Font, brush, e:CellBounds, sf)
			brush:Dispose()
			sf:Dispose()
			e:Handled := TRUE

		ENDIF
		RETURN
	METHOD RenderColumnHeader(g AS System.Drawing.Graphics , headerBounds AS System.Drawing.Rectangle, c AS System.Drawing.Color )  AS VOID
		LOCAL topHeight AS INT
		LOCAL IMPLIED topRect	  := System.Drawing.Rectangle{headerBounds:Left, headerBounds:Top+1, headerBounds:Width, topHeight}
		LOCAL IMPLIED bottomRect  := RectangleF{headerBounds:Left, headerBounds:Top+1 + topHeight, headerBounds:Width, ;
									headerBounds:Height- topHeight-4}
		LOCAL IMPLIED c1 := System.Drawing.Color.DarkGray
		LOCAL IMPLIED brush := System.Drawing.SolidBrush{c1}

			g:FillRectangle(brush, topRect)
			brush:Color := c
			g:FillRectangle(brush, bottomRect)
		brush:Dispose()
		RETURN

		METHOD RenderColumnHeaderBorder(g AS Graphics , headerBounds AS Rectangle, colindex AS INT )  AS VOID
		g:DrawRectangle(System.Drawing.Pen{System.Drawing.Color.White, 0.1}, Convert.ToSingle((headerBounds:Left + 0.5)), Convert.ToSingle((headerBounds:Top + 0.5)),;
					(REAL4)(headerBounds:Width-1),(REAL4)(headerBounds:Height-1))
		ControlPaint.DrawBorder(g, headerBounds, System.Drawing.Color.Gray, 0, ButtonBorderStyle.Inset, ;
											   System.Drawing.Color.Gray, 0, ButtonBorderStyle.Inset, ;
											 System.Drawing.Color.DarkGray, iif(colindex != __DataGridView:ColumnCount - 1 , 1 , 0), ;
											 ButtonBorderStyle.Inset, System.Drawing.Color.DarkGray, 1, ButtonBorderStyle.Inset)
	RETURN
/*
	//CellPainting event handler for your dataGridView1
	private void dataGridView1_CellPainting(object sender, DataGridViewCellPaintingEventArgs e){
		if (e.RowIndex == -1 && e.ColumnIndex > -1){
			e.PaintBackground(e.CellBounds, true);
			RenderColumnHeader(e.Graphics, e.CellBounds, e.CellBounds.Contains(hotSpot) ? hotSpotColor : backColor);
			RenderColumnHeaderBorder(e.Graphics, e.CellBounds, e.ColumnIndex);
			using (Brush brush = new SolidBrush(e.CellStyle.ForeColor)){
				using (StringFormat sf = new StringFormat() {LineAlignment = StringAlignment.Center, Alignment = StringAlignment.Center}) {
					e.Graphics.DrawString(e.Value.ToString(), e.CellStyle.Font, brush, e.CellBounds, sf);
				}
			}
			e.Handled = true;
		}
	}
	Color hotSpotColor = Color.LightGreen;//For hover backcolor
	Color backColor = Color.LimeGreen;    //For backColor
	Point hotSpot;
	private void RenderColumnHeader(Graphics g, Rectangle headerBounds, Color c) {
		int topHeight = 10;
		Rectangle topRect = new Rectangle(headerBounds.Left, headerBounds.Top+1, headerBounds.Width, topHeight);
		RectangleF bottomRect = new RectangleF(headerBounds.Left, headerBounds.Top+1 + topHeight, headerBounds.Width, headerBounds.Height- topHeight-4);
		Color c1 = Color.FromArgb(180, c);
		using (SolidBrush brush = new SolidBrush(c1)) {
			g.FillRectangle(brush, topRect);
			brush.Color = c;
			g.FillRectangle(brush, bottomRect);
		}
	}
	private void RenderColumnHeaderBorder(Graphics g, Rectangle headerBounds, int colIndex) {
		g.DrawRectangle(new Pen(Color.White, 0.1f), headerBounds.Left + 0.5f, headerBounds.Top + 0.5f,headerBounds.Width-1f,headerBounds.Height-1f);
		ControlPaint.DrawBorder(g, headerBounds, Color.Gray, 0, ButtonBorderStyle.Inset,
											   Color.Gray, 0, ButtonBorderStyle.Inset,
											 Color.Gray, colIndex != dataGridView1.ColumnCount - 1 ? 1 : 0, ButtonBorderStyle.Inset,
										   Color.Gray, 1, ButtonBorderStyle.Inset);
	}
*/
END CLASS

/// <include file="Gui.xml" path="doc/DataColumn/*" />
CLASS DataColumn INHERIT VObject
	PROTECT iDataField AS INT

	PROTECT lModified AS LOGIC
	PROTECT lDefaultWidth AS LOGIC
	PROTECT lBaseServer AS LOGIC
	PROTECT lExplicitFS AS LOGIC
	PROTECT lExplicitHL AS LOGIC
	PROTECT lChanged AS LOGIC

	PROTECT cPicture AS STRING
	PROTECT cCaption AS STRING
	PROTECT cTextValue AS STRING

	PROTECT symUserDrawMethod AS SYMBOL
	PROTECT symDataField AS SYMBOL

	PROTECT oParent AS DataBrowser
	PROTECT oFieldSpec AS FieldSpec
	PROTECT oDataField AS DataField
	PROTECT oServer AS DataServer
	PROTECT oHyperLabel AS HyperLabel
	PROTECT oHlStatus AS HyperLabel

	PROTECT cbGetSetBlock AS USUAL
	PROTECT uGetSetOwner AS USUAL
	PROTECT uValue AS USUAL
	INTERNAL oDataGridColumn AS VODataGridViewColumn
	INTERNAL dwWidth AS DWORD

 /// <exclude />

	METHOD __CreateColumn AS VODataGridViewColumn
		RETURN VODataGridViewColumn{SELF}

 /// <exclude />
	ACCESS __Column AS VODataGridViewColumn
		RETURN oDataGridColumn

 /// <exclude />
	ACCESS __DataField AS DataField STRICT
		RETURN oDataField
 /// <exclude />
	METHOD __Gather() AS DataColumn STRICT
		LOCAL oHL AS HyperLabel
		LOCAL oWin AS Window

		SELF:__Update()

		IF lChanged
			IF (oServer != NULL_OBJECT)
				IF lBaseServer // if not subclassing
					oServer:FIELDPUT(iDataField,SELF:Value) //use FieldPut
				ELSEIF symDataField!=NULL_SYMBOL
					IVarPut(oServer, symDataField ,SELF:Value)
				ELSE
					IVarPut(oServer, SELF:NameSym ,SELF:Value)
				ENDIF
				oHL := oServer:Status
				IF (oHL != NULL_OBJECT) .AND. (oParent != NULL_OBJECT)
					oWin := (Window) oParent:Owner
					IF oWin IS AppWindow VAR oAppWin
						oAppWin:@@StatusMessage((ResourceString{__WCSError2}:Value)+oHL:Description,MESSAGEERROR)
					ENDIF
				ENDIF
			ELSEIF !IsNil(cbGetSetBlock) .AND. IsCodeBlock(cbGetSetBlock)
				Eval(cbGetSetBlock, uGetSetOwner, SELF:Value)
			ENDIF
		ENDIF

		RETURN SELF


 /// <exclude />
	ASSIGN __Owner(oDB AS DataBrowser)  STRICT
		IF oDB!=NULL_OBJECT
			IF oParent==NULL_OBJECT
				oParent:=oDB
			ENDIF
		ELSE
			oParent:=NULL_OBJECT
		ENDIF
		RETURN


 /// <exclude />
	ACCESS __Picture AS STRING STRICT
		RETURN cPicture


 /// <exclude />
   METHOD __Scatter() AS VOID STRICT
	//PP-030828 Strong typing




	  IF oServer IS DataServer
		 IF lBaseServer // if not subclassing
			SELF:Value := oServer:FIELDGET(symDataField) //use fieldget
		 ELSEIF symDataField != NULL_SYMBOL
			SELF:Value:=IVarGet(oServer, symDataField)
		 ELSE
			SELF:Value:=IVarGet(oServer, SELF:NameSym)
		 ENDIF
	  ELSEIF !IsNil(cbGetSetBlock) .AND. IsCodeBlock(cbGetSetBlock)
		 SELF:Value := Eval(cbGetSetBlock, uGetSetOwner)
	  ENDIF
		SELF:lChanged := FALSE
		RETURN

	#region Obsolete Methods
 /// <exclude />
	[Obsolete];
	METHOD __SetFldColor(oDataBrowser AS DataBrowser, iLoc AS INT, dwClr AS DWORD) AS VOID STRICT
		RETURN
	#endregion

 /// <exclude />
	METHOD __UnLink(oDS := NIL AS USUAL) AS VOID STRICT
		// Do actual unlinking
		IF IsNil(oDS)
			uGetSetOwner := NIL
			cbGetSetBlock := NIL
			oDataField:= NULL_OBJECT
			oServer:= NULL_OBJECT
		ELSE
			IF (oDS == oServer)
				oDataField:= NULL_OBJECT
				oServer:= NULL_OBJECT
			ENDIF
		ENDIF
		RETURN


/// <exclude />
	METHOD __Update() AS VOID  STRICT
		// force update to container
		LOCAL cText AS STRING
		LOCAL uOldValue AS USUAL

		IF lModified
			cText := SELF:TextValue
			uOldValue := uValue
			IF (oFieldSpec != NULL_OBJECT)
				uValue := oFieldSpec:Val(cText)
				SELF:TextValue := oFieldSpec:Transform(uValue)
			ELSE
				uValue := cText
			ENDIF
			SELF:Modified := FALSE

			IF !(uOldValue == uValue) // dont change to !=, might be STRING !!!
				SELF:lChanged := TRUE
			ENDIF
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/DataColumn.Alignment/*" />
	ACCESS Alignment AS LONG
		LOCAL IMPLIED nALignment := SELF:oDataGridColumn:CellTemplate:Style:Alignment
		LOCAL iRet as LONG
		SWITCH nALignment
		CASE DataGridViewContentAlignment.MiddleCenter
			iRet := gbaAlignCenter
		CASE DataGridViewContentAlignment.MiddleLeft
			iRet := gbaAlignLeft
		CASE DataGridViewContentAlignment.MiddleRight
			iRet := gbaAlignRight
		END SWITCH
		RETURN iRet

/// <include file="Gui.xml" path="doc/DataColumn.Alignment/*" />
	ASSIGN Alignment (nNewAlign AS LONG)
		LOCAL iAlign AS INT

		iAlign := nNewAlign

		SWITCH iAlign
		CASE gbaAlignCenter
			SELF:oDataGridColumn:CellTemplate:Style:Alignment := DataGridViewContentAlignment.MiddleCenter
			SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleCenter
		CASE gbaAlignLeft

			SELF:oDataGridColumn:CellTemplate:Style:Alignment := DataGridViewContentAlignment.MiddleLeft
			SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleLeft
		CASE gbaAlignRight
			SELF:oDataGridColumn:CellTemplate:Style:Alignment := DataGridViewContentAlignment.MiddleRight
			SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleRight
		END SWITCH

		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.AsString/*" />
	METHOD AsString(uParam)
		LOCAL cString AS STRING
		LOCAL uVal2Print AS USUAL
		LOCAL lHasPic AS LOGIC

		lHasPic := (oFieldSpec != NULL_OBJECT) .AND. SLen(oFieldSpec:Picture) > 0

		IF IsNil(uParam)
			uVal2Print := uValue
		ELSE
			uVal2Print := uParam
		ENDIF

		IF IsNil(uVal2Print)
			RETURN NULL_STRING
		ENDIF

		IF lHasPic
			cString := oFieldSpec:Transform(uVal2Print)
		ELSEIF (oFieldSpec != NULL_OBJECT) .AND. (oFieldSpec:ValType == "N")
			cString := Str(uVal2Print, oFieldSpec:Length, oFieldSpec:Decimals)
		ELSE
			cString := _AsString(uVal2Print)
		ENDIF

		RETURN cString

/// <include file="Gui.xml" path="doc/DataColumn.Background/*" />
	ACCESS Background AS VOSDK.Brush
		RETURN VOSDK.Brush{ (VOSDK.Color) SELF:oDataGridColumn:DefaultCellStyle:BackColor}

/// <include file="Gui.xml" path="doc/DataColumn.Background/*" />
	ASSIGN Background(oBrush AS VOSDK.Brush)
		SELF:ChangeBackground(oBrush, gblText)

		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.Block/*" />
	ACCESS Block  AS USUAL
		RETURN cbGetSetBlock

/// <include file="Gui.xml" path="doc/DataColumn.Block/*" />
	ASSIGN Block(aCb AS USUAL)
		IF !Empty(aCB) .AND. IsCodeBlock(aCB)
			cbGetSetBlock := aCb

			// reset data server connection
			oDataField:= NULL_OBJECT
			oServer:= NULL_OBJECT
			iDataField := 0
		ELSE
			cbGetSetBlock := NULL_CODEBLOCK
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.BlockOwner/*" />
	ACCESS BlockOwner  AS USUAL
		RETURN uGetSetOwner

/// <include file="Gui.xml" path="doc/DataColumn.BlockOwner/*" />
	ASSIGN BlockOwner(xOwner AS USUAL)
		IF !IsNil(xOwner)
			uGetSetOwner := xOwner
		ELSE
			uGetSetOwner := NIL
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.Caption/*" />
	ACCESS Caption AS STRING
		RETURN cCaption

/// <include file="Gui.xml" path="doc/DataColumn.Caption/*" />
	ASSIGN Caption(cNewCaption AS STRING)
		SELF:SetCaption(cNewCaption)
		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.CellBackground/*" />
	ACCESS CellBackground  AS VOSDK.Brush
		RETURN SDK.Brush{ (VOSDK.Color) SELF:oDataGridColumn:DefaultCellStyle:BackColor}

/// <include file="Gui.xml" path="doc/DataColumn.CellBackground/*" />
	ASSIGN CellBackground(oBrush AS VOSDK.Brush)
		SELF:oDataGridColumn:DefaultCellStyle:BackColor := oBrush:Color

/// <include file="Gui.xml" path="doc/DataColumn.CellTextColor/*" />
	ACCESS CellTextColor AS VOSDK.Color
		RETURN SELF:oDataGridColumn:DefaultCellStyle:ForeColor

/// <include file="Gui.xml" path="doc/DataColumn.CellTextColor/*" />
	ASSIGN CellTextColor(oColor AS VOSDK.Color)
		SELF:oDataGridColumn:DefaultCellStyle:ForeColor := oColor

/// <include file="Gui.xml" path="doc/DataColumn.ChangeBackground/*" />
	METHOD ChangeBackground(oBrush AS Brush, kWhere AS LONG) AS Brush
		LOCAL oOldBrush AS SDK.Brush
		LOCAL oOldCol   AS SDK.Color
		LOCAL oNewBrush AS SDK.Brush

		oNewBrush := oBrush

		SWITCH kWhere
		CASE gblCaption
		CASE gblColCaption
			oOldCol := SELF:oDataGridColumn:HeaderCell:Style:BackColor
			oOldBrush := VOSDK.Brush{oOldCol}
			SELF:oDataGridColumn:HeaderCell:Style:BackColor := oNewBrush:Color

		CASE gblText
			oOldCol := SELF:oDataGridColumn:DefaultCellStyle:BackColor
			oOldBrush := VOSDK.Brush{oOldCol}
			SELF:oDataGridColumn:DefaultCellStyle:BackColor := oNewBrush:Color

		CASE gblButton
		CASE gblColButton
			// Not supported
			NOP
		END SWITCH

		RETURN oOldBrush

	METHOD ChangeTextColor(oColor AS LONG, kWhere AS LONG) AS Color
		RETURN SELF:ChangeTextColor(Color{(DWORD) oColor}, kWhere)

/// <include file="Gui.xml" path="doc/DataColumn.ChangeTextColor/*" />
	METHOD ChangeTextColor(oColor AS Color, kWhere AS LONG) AS Color
		LOCAL oOldColor AS VOSDK.Color
		LOCAL oNewColor AS VOSDK.Color

		SWITCH kWhere
		CASE gblCaption
		CASE gblColCaption
			oOldColor := SELF:oDataGridColumn:HeaderCell:Style:ForeColor
			SELF:oDataGridColumn:HeaderCell:Style:ForeColor := oNewColor

		CASE gblText
			oOldColor := SELF:oDataGridColumn:DefaultCellStyle:ForeColor
			SELF:oDataGridColumn:DefaultCellStyle:ForeColor := oNewColor

		CASE gblButton
		CASE gblColButton
			// Not supported
			NOP
		END SWITCH
		//SELF:__SetFldColor(NULL_OBJECT, iLoc, oNewColor:ColorRef)

		RETURN oOldColor

/// <include file="Gui.xml" path="doc/DataColumn.ClearStatus/*" />
	METHOD ClearStatus() AS VOID
		SELF:oHlStatus := NULL_OBJECT
		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.DataField/*" />
	ACCESS DataField AS SYMBOL
		RETURN SELF:symDataField

/// <include file="Gui.xml" path="doc/DataColumn.Destroy/*" />
	METHOD Destroy() AS USUAL CLIPPER
		IF SELF:oDataGridColumn != NULL_OBJECT
			SELF:oDataGridColumn:Dispose()
			GC.SuppressFinalize(SELF:oDataGridColumn)
			oDataGridColumn := NULL_OBJECT
		ENDIF
		SUPER:Destroy()

		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.DisableCellDraw/*" />
	METHOD DisableCellDraw()


		//CntFldAttrClear( strucFI, CFA_OWNERDRAW)
		//lUsingDrawProc:=FALSE

		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.DrawCellData/*" />
	METHOD DrawCellData(uValue)
		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.EnableCellDraw/*" />
	METHOD EnableCellDraw(symMethodName)
		IF !IsNil(symMethodName)
			IF !IsSymbol(symMethodName)
				WCError{#EnableCellDraw,#DataColumn,__WCSTypeError,symMethodName,1}:Throw()
			ENDIF
			symUserDrawMethod := symMethodName
		ELSE
			symUserDrawMethod := NULL_SYMBOL
		ENDIF
		// Todo ?
		//IF !lUsingDrawProc
		//	lUsingDrawProc := TRUE
		//	CntFldAttrSet( strucFI, CFA_OWNERDRAW)
		//	IF DrawFldDataDelegate == NULL
		//		DrawFldDataDelegate := __DrawFldDataDelegate{ NULL, @__DrawFldData() }
		//	ENDIF

		//	CntFldDrwProcSet( strucFI, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) DrawFldDataDelegate ) )
		//ENDIF

		IF (oParent != NULL_OBJECT)
			oParent:Refresh()
		ENDIF

		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.EnableColumnMove/*" />
	METHOD EnableColumnMove(lAllowMove := TRUE AS LOGIC)

		// Does not work
		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.EnableColumnReSize/*" />
	METHOD EnableColumnReSize(lAllowResize:= TRUE AS LOGIC)

		IF lAllowResize
			SELF:oDataGridColumn:Resizable := DataGridViewTriState.True
		ELSE
			SELF:oDataGridColumn:Resizable := DataGridViewTriState.False
		ENDIF

		RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.FieldSpec/*" />
	ACCESS FieldSpec  AS FieldSpec
		RETURN oFieldSpec

/// <include file="Gui.xml" path="doc/DataColumn.FieldSpec/*" />
	ASSIGN FieldSpec(oFS AS FieldSpec)

		oFieldSpec := oFS
		lExplicitFS := TRUE
		cPicture := oFieldSpec:Picture

		IF oFieldSpec:ValType == "N"
			IF SubStr(cPicture, 1, 2) == "@B"
				SELF:Alignment := GBAALIGNLEFT
			ELSE
				SELF:Alignment := GBAALIGNRIGHT
			ENDIF
		ENDIF

		// We need to update the column if is visible and
		// connected to the server

		IF oServer!=NULL_OBJECT .AND. oDataField!=NULL_OBJECT .OR. !IsNil(cbGetSetBlock)
			SELF:__Scatter()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.GetCaption/*" />
	METHOD GetCaption() AS STRING
		RETURN cCaption

/// <include file="Gui.xml" path="doc/DataColumn.GetEditObject/*" />
	METHOD GetEditObject(oOwner, iID, oPoint, oDim)
		LOCAL oControl AS TextControl
        local oDb as DataBrowser
        oDb := oOwner
		oControl := SingleLineEdit{oOwner, iID, oPoint, oDim, ES_AUTOHSCROLL}
		IF (oFieldSpec != NULL_OBJECT)
			oControl:FieldSpec := oFieldSpec
		ENDIF
		IVarPut(oControl, #Overwrite, OVERWRITE_ONKEY)

		oControl:SetExStyle(WS_EX_CLIENTEDGE, FALSE)
		oControl:Font(oDb:EditFont, false)
		oControl:TextValue := RTrim(SELF:TextValue)
		//SendMessage(oControl:Handle(), EM_SETSEL, 0, -1)

		RETURN oControl

/// <include file="Gui.xml" path="doc/DataColumn.GetModified/*" />
	METHOD GetModified() AS LOGIC STRICT
		RETURN lModified

/// <include file="Gui.xml" path="doc/DataColumn.GetValue/*" />
	METHOD GetValue() AS STRING STRICT
		RETURN cTextValue

/// <include file="Gui.xml" path="doc/DataColumn.HyperLabel/*" />
	access HyperLabel as HyperLabel
		RETURN oHyperLabel

/// <include file="Gui.xml" path="doc/DataColumn.HyperLabel/*" />
	assign HyperLabel(oNewHL as HyperLabel)

		oHyperLabel := oNewHL
		lExplicitHL := true
		self:Caption := oHyperLabel:Caption
// 		elseif IsString(oNewHL)
// 			oHyperLabel := HyperLabel{String2Symbol(oNewHL)}
// 			lExplicitHL := TRUE
// 			SELF:Caption := oHyperLabel:Caption
// 		ELSEIF IsSymbol(oNewHL)
// 			oHyperLabel := HyperLabel{oNewHL}
// 			lExplicitHL := TRUE
// 			SELF:Caption := oHyperLabel:Caption
// 		ELSEIF IsNil(oNewHL)
// 			oHyperLabel := NULL_OBJECT
// 			lExplicitHL := FALSE
// 			// Should we reset the caption ??
// 		ELSE
// 			WCError{#HyperLabel,#DataColumn,__WCSTypeError,oNewHL,1}:Throw()
// 		ENDIF
		return

/// <include file="Gui.xml" path="doc/DataColumn.ctor/*" />
	CONSTRUCTOR(nWidth, xColumnID)
		LOCAL iSize AS INT

		SUPER()
		oDataGridColumn := SELF:__CreateColumn()

		IF IsNil(nWidth)
			iSize := 16
		ELSEIF nWidth IS FieldSpec
			iSize := __GetFSDefaultLength(nWidth)
			SELF:FieldSpec := nWidth
		ELSEIF !IsLong(nWidth)
			WCError{#Init,#DataColumn,__WCSTypeError,nWidth,1}:Throw()
		ELSE
			iSize := nWidth
		ENDIF

		cTextValue := "N/A"

		IF xColumnID IS HyperLabel
			SELF:HyperLabel := xColumnID
		ELSEIF IsString(xColumnID)
			SELF:HyperLabel := HyperLabel{String2Symbol(xColumnID),xColumnID}
		ELSEIF IsSymbol(xColumnID)
			SELF:HyperLabel := HyperLabel{xColumnID,Symbol2String(xColumnID)}
		ENDIF

		//// Set up default width
		lDefaultWidth := TRUE

		IF iSize != -1
			dwWidth  := DWORD(iSize + 2)
		ELSE
			dwWidth := 0XFFFFFFFF
		ENDIF


		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.LinkDF/*" />
	METHOD LinkDF(oDataServer, nFieldData)
		LOCAL tmpDF AS OBJECT
		LOCAL symClassName AS SYMBOL
		IF !(oDataServer IS DataServer)
			WCError{#LinkDF,#DataColumn,__WCSTypeError,oDataServer,1}:Throw()
		ENDIF

		IF !IsNil(oServer) .AND. oDataServer!=oServer
			SELF:__UnLink()
		ENDIF

		oServer := oDataServer
		iDataField := nFieldData
		symDataField := oServer:FieldSym(iDataField)
		IF IsMethod(oServer, #IsBaseField)
			lBaseServer := Send(oServer, #IsBaseField, symDataField)
		ELSE
			symClassName := ClassName(oServer)
			lBaseServer  := symClassName==#DBServer .OR. symClassName==#SQLSelect .OR. symClassName==#SQLTable .OR. symClassName==#JDataServer
		ENDIF

		// Propogate data field if no explicit one
		tmpDF:=oServer:DataField(iDataField)
		IF tmpDF IS DataField
			oDataField := tmpDF

			IF !lExplicitFS
				// propogate field spec if no explicit one
				oFieldSpec := oDataField:FieldSpec

				// propogate field spec
				IF !lExplicitHL
					// CHECK IF NameSym and hyperlabel are same
					IF !IsNil(oDataField:HyperLabel) .AND. (oDataField:NameSym == oDataField:HyperLabel:NameSym)
						oHyperLabel := oDataField:HyperLabel
					ELSE
						IF cCaption==NULL_STRING
							oHyperLabel := HyperLabel {oDataField:NameSym}
						ELSE
							oHyperLabel := HyperLabel {oDataField:NameSym,cCaption}
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF

		uGetSetOwner:= NIL
		cbGetSetBlock:= NIL

		// Get initial value
		SELF:__Scatter()

		RETURN NIL

/// <include file="Gui.xml" path="doc/DataColumn.Modified/*" />
	ACCESS Modified AS LOGIC
		RETURN lModified

/// <include file="Gui.xml" path="doc/DataColumn.Modified/*" />
	ASSIGN Modified(lChangedFlag AS LOGIC)
		lModified := lChangedFlag
		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.Name/*" />
	ACCESS Name AS STRING
		IF oHyperLabel!=NULL_OBJECT
			RETURN oHyperLabel:Name
		ENDIF

		RETURN NULL_STRING

/// <include file="Gui.xml" path="doc/DataColumn.NameSym/*" />
	ACCESS NameSym AS SYMBOL

	  IF oHyperLabel!=NULL_OBJECT
		 RETURN oHyperLabel:NameSym
	  ENDIF

	  RETURN NULL_SYMBOL

/// <include file="Gui.xml" path="doc/DataColumn.Owner/*" />
	ACCESS Owner as DataBrowser

	  RETURN oParent

/// <include file="Gui.xml" path="doc/DataColumn.PerformValidations/*" />
	METHOD PerformValidations() AS LOGIC
		// Perform validations for DataColumn against supplied parameter
		// if it has a data spec, otherwise just return TRUE

		IF (oFieldSpec != NULL_OBJECT)
			IF !oFieldSpec:PerformValidations(uValue)
				oHlStatus := oFieldSpec:Status
				RETURN FALSE
			ENDIF
		ENDIF
		oHlStatus:= NULL_OBJECT

		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataColumn.PixelWidth/*" />
	ACCESS PixelWidth AS LONG
		RETURN SELF:oDataGridColumn:Width

/// <include file="Gui.xml" path="doc/DataColumn.Server/*" />
	ACCESS Server AS DataServer
	  RETURN oServer

/// <include file="Gui.xml" path="doc/DataColumn.SetCaption/*" />
	METHOD SetCaption(cText AS STRING, kAlignment:=gbaAlignCenter AS LONG) AS LOGIC
		LOCAL iLines AS INT
		LOCAL iMaxWidth AS INT


		// calculate width based on caption
		IF Slen(cText) > 0
			VAR aLines := cText:Split(<CHAR>{'\r', '\n'}, StringSplitOptions.RemoveEmptyEntries)
			iLines := aLines:Length
			FOREACH line AS STRING IN aLines
				iMaxWidth := Max((INT) SLen(line), iMaxWidth)
			NEXT

			// multi line titles are vertically aligned on the Top of the control by default
			IF (iLines > 1)
				SWITCH kAlignment
				CASE GBAALIGNCENTER
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.TopCenter
				CASE GBAALIGNLEFT
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.TopLeft
				CASE GBAALIGNRIGHT
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.TopRight
				END SWITCH
			ELSE
				SWITCH kAlignment
				CASE GBAALIGNCENTER
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleCenter
				CASE GBAALIGNLEFT
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleLeft
				CASE GBAALIGNRIGHT
					SELF:oDataGridColumn:HeaderCell:Style:Alignment := DataGridViewContentAlignment.MiddleRight
				END SWITCH
			ENDIF

			cCaption := cText

			// default width if necessary
			IF lDefaultWidth .AND. (INT)SELF:dwWidth < (iMaxWidth + 2)
				dwWidth := (DWORD) iMaxWidth +2
			ELSEIF dwWidth == 0XFFFFFFFF
				dwWidth := (DWORD) iMaxWidth +2
			ENDIF
		ELSE
			cCaption := NULL_STRING
		ENDIF
		SELF:oDataGridColumn:HeaderText := cCaption
		IF SELF:dwWidth != 0XFFFFFFFF
			SELF:Width :=  (INT) dwWidth
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/DataColumn.SetModified/*" />
	METHOD SetModified(lModified)
		IF !IsLogic(lModified)
			WCError{#SetModified,#DataColumn,__WCSTypeError,lModified,1}:Throw()
		ENDIF
		SELF:lModified:=lModified


	  RETURN SELF

/// <include file="Gui.xml" path="doc/DataColumn.SetStandardStyle/*" />
	METHOD SetStandardStyle(kStyle AS LONG)  AS VOID

		SWITCH kStyle
		CASE GBSREADONLY
			oDataGridColumn:ReadOnly := TRUE

		CASE GBSEDIT
			oDataGridColumn:ReadOnly := FALSE

		CASE GBSCONTROL3D
			// Not implemented
			NOP
		CASE GBSCONTROL2D
			// Not implemented
			NOP
		END SWITCH

		RETURN


/// <include file="Gui.xml" path="doc/DataColumn.SetValue/*" />
	METHOD SetValue(cNewValue AS STRING)
		IF !(cNewValue == cTextValue)
			SELF:TextValue := cNewValue
			SELF:Modified := TRUE
			SELF:__Update()
			// We have to set lChanged here
			// The textvalue assign already takes care of updating uValue and therefore
			// the __Update method does not set lChanged
			SELF:lChanged := TRUE
		ENDIF

		IF SELF:ValueChanged
			IF !SELF:PerformValidations()
				oHlStatus := SELF:Status
				oParent:__FieldChange()
			ELSE
				SELF:__Gather()
			ENDIF
		ENDIF

		RETURN cTextValue


/// <include file="Gui.xml" path="doc/DataColumn.Status/*" />
	ACCESS Status AS HyperLabel
		RETURN oHlStatus

/// <include file="Gui.xml" path="doc/DataColumn.TextColor/*" />
	ACCESS TextColor AS VOSDK.Color
		RETURN (VOSDK.Color) SELF:oDataGridColumn:DefaultCellStyle:ForeColor


/// <include file="Gui.xml" path="doc/DataColumn.TextColor/*" />
	ASSIGN TextColor(oColor AS VOSDK.Color)
		SELF:ChangeTextColor(oColor, gblText)
		RETURN

/// <include file="Gui.xml" path="doc/DataColumn.TextValue/*" />
	ACCESS TextValue AS STRING
		RETURN cTextValue


/// <include file="Gui.xml" path="doc/DataColumn.TextValue/*" />
	ASSIGN TextValue(sNewText  AS STRING)
		cTextValue := Trim(sNewText)
		IF (oFieldSpec != NULL_OBJECT)
			uValue := oFieldSpec:Val(cTextValue)
		ELSE
			uValue := cTextValue
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/DataColumn.Value/*" />
	ACCESS Value AS USUAL
		RETURN uValue

/// <include file="Gui.xml" path="doc/DataColumn.Value/*" />
	ASSIGN Value(uParm AS USUAL)
		LOCAL cTemp AS STRING

		IF IsNil(uParm)
			uValue:= NIL
			SELF:TextValue := NULL_STRING
			oHlStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ELSE
			IF (oFieldSpec != NULL_OBJECT)
				cTemp := oFieldSpec:Transform(uParm)
			ELSE
				cTemp := AsString(uParm)
			ENDIF

			uValue := uParm
			SELF:TextValue := cTemp
			SELF:oHlStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ENDIF

	  RETURN
/// <include file="Gui.xml" path="doc/DataColumn.ValueChanged/*" />
	ACCESS ValueChanged AS LOGIC
		RETURN lChanged

/// <include file="Gui.xml" path="doc/DataColumn.ValueChanged/*" />
	ASSIGN ValueChanged(lNewFlag AS LOGIC)
		lChanged := lNewFlag
		RETURN
/// <include file="Gui.xml" path="doc/DataColumn.VisualPos/*" />

	ACCESS VisualPos AS LONG
		RETURN SELF:oDataGridColumn:DisplayIndex+1
/// <include file="Gui.xml" path="doc/DataColumn.Width/*" />

	ACCESS Width AS LONG
		RETURN SELF:oDataGridColumn:Width / 6

/// <include file="Gui.xml" path="doc/DataColumn.Width/*" />
	ASSIGN Width(nNewWidth AS LONG)
		IF nNewWidth == 0
			SELF:oDataGridColumn:Visible := FALSE
		ELSE
			SELF:oDataGridColumn:Visible := TRUE
			SELF:oDataGridColumn:Width := nNewWidth * 6
		ENDIF
		RETURN
END CLASS


#define VSCROLLMIN	0
#define VSCROLLMAX	1000
#define VSCROLLOFFSET 100

INTERNAL CLASS DataBrowserScrollBarManager
	// This class manages the synchronization between the visible position in the grid and the scrollbar value
	// The Scrollbar has its range set to 0-1000
	// There are a couple of situations that make this special
	// 1) When the Grid contains all the rows from the underlying table then a simple calculation takes place
	// 2) When the Grid "misses" the beginning of the data, then the values 0-99 from the Scrollbar are "virtual" and
	//    then value 100 of the scrollbar matches row 0 in the grid
	// 3) When the Grid "misses" the end of the data, then the values 901-1000 from the Scrollbar a "virtual" and
	//    then value 900 of the scrollbar matches the last row in the grid

	PROTECT _oGridView	AS VODataGridView
	PROTECT _oScrollBar	AS VScrollBar
	PROTECT _oBrowser	AS DataBrowser
	PROTECT _nStart		AS LONG
	PROTECT _nEnd		AS LONG

	CONSTRUCTOR (oBrowser AS DataBrowser, oScrollBar AS VScrollBar, oGrid AS VODataGridView)
		_oBrowser   := oBrowser
		_oScrollBar := oScrollBar
		_oGridView	:= oGrid
		_oScrollBar:Scroll      += OnVScrolled
		_nStart := _oScrollBar:Minimum	:= VSCROLLMIN
		_nEnd   := _oScrollBar:Maximum := VSCROLLMAX

		RETURN

	PROTECTED METHOD SetRange() AS VOID
		IF _oBrowser:HasTop
			_nStart := _oScrollBar:Minimum
		ELSE
			_nStart := _oScrollBar:Minimum + VSCROLLOFFSET
		ENDIF
		IF _oBrowser:HasBottom
			_nEnd  := _oScrollBar:Maximum
		ELSE
			_nEnd  := _oScrollBar:Maximum - VSCROLLOFFSET
		ENDIF
		RETURN

	PROPERTY IsValueVirtual AS LOGIC GET _oScrollBar:Value < _nStart .or. _oScrollBar:Value > _nEnd
	PROPERTY IsScoped		AS LOGIC GET _nStart != _oScrollBar:Minimum .or. _nEnd != _oScrollBar:Maximum

	PROTECTED METHOD OnVScrolled(sender AS OBJECT, se AS ScrollEventArgs ) AS VOID
		LOCAL nNew, nOld AS LONG
		LOCAL currentIndex	AS LONG
		SELF:SetRange()
		nNew := se:NewValue
		nOld := se:OldValue
		DO CASE
		CASE _oBrowser:Server == NULL_OBJECT
			RETURN
		CASE se:Type == ScrollEventType.Last
			IF ! _oBrowser:HasBottom
				// Load end of buffer
				_oBrowser:Server:GoBottom()
			ENDIF
			_oGridView:TopRowIndex := _oGridView:Rows:Count - _oGridView:VisibleRows

		CASE se:Type == ScrollEventType.First
			IF _oBrowser:HasTop
				_oBrowser:Server:GoTop()
			ENDIF
			_oGridView:TopRowIndex := 0

		//CASE nNew <= _nEnd .and. nNew >= _nStart
		//	SELF:SyncGrid(nNew)
		//CASE nNew < nOld .and. nNew <= _nStart
		//	IF ! _oBrowser:HasTop
		//		oRow := _oGridView:Rows[currentIndex]
		//		_oBrowser:__DeltaBuildBufferUp()
		//	ENDIF
		//	SELF:SyncGrid(nNew)


		CASE se:Type == ScrollEventType.SmallDecrement
			SELF:ScrollUp(_oScrollBar:SmallChange)
			se:NewValue := _oScrollBar:Value
		CASE se:Type == ScrollEventType.SmallIncrement
			SELF:ScrollDown(_oScrollBar:SmallChange)
			se:NewValue := _oScrollBar:Value
		CASE se:Type == ScrollEventType.LargeDecrement
			SELF:ScrollUp(_oScrollBar:LargeChange)
			se:NewValue := _oScrollBar:Value
		CASE se:Type == ScrollEventType.LargeIncrement
			SELF:ScrollDown(_oScrollBar:LargeChange)
			se:NewValue := _oScrollBar:Value
		CASE se:Type == ScrollEventType.ThumbPosition
			IF nNew >= _nStart .and. nNew <= _nEnd
				SELF:SyncGrid(nNew)
			ELSE
				// Stay on the same row
                NOP
			ENDIF
		CASE se:Type == ScrollEventType.ThumbTrack
			// This happens when they drag the thumb up and down
			// When they drag the thumb up we need to check if we have the top.
			// When not and they drag in the "virtual" area before the first record, then
			// we must load new rows on top
			// When they drag the thumb down into the "virtual" area after the last record then
			// We heed to extend the buffer at the end
			// nNew will be a number between VSCROLLMIN and VSCROLLMAX
			currentIndex := _oGridView:TopRowIndex

			IF nNew < nOld
				IF nNew < _nStart
					// drag into virtual area at the top, so load records before first row in buffer
					_oBrowser:__DeltaBuildBufferUp()
					_oGridView:TopRowIndex := 0
				ENDIF
			ELSEIF nNew > nOld
				IF currentIndex > SELF:_oGridView:Rows:Count
					_oBrowser:__DeltaBuildBufferDown()
				ENDIF
			ENDIF
			SELF:SyncGrid(nNew)
		CASE se:Type == ScrollEventType.EndScroll
			// Do nothing
			//SELF:SyncGrid(nNew)
            NOP

		ENDCASE
		RETURN


	METHOD ScrollUp(nLines AS LONG) AS VOID
		LOCAL currentIndex	AS LONG
		LOCAL nRecno		AS LONG
		LOCAL oRow			AS VODataGridViewRow
		currentIndex := _oGridView:TopRowIndex
		IF currentIndex > nLines
			_oGridView:TopRowIndex := currentIndex - nLines
		ELSEIF _oBrowser:HasTop
			// Do Nothing since we are at the begin of the data
			_oGridView:TopRowIndex := 0
		ELSE
			oRow	 := (VODataGridViewRow) _oGridView:Rows[currentIndex]
			nRecno   := oRow:RecNo
			_oBrowser:__DeltaBuildBufferUp()
			oRow     := _oBrowser:__GetRowAtRecNo(nRecno)
			_oGridView:TopRowIndex := oRow:Index - nLines
		ENDIF
		SELF:SyncScrollBar()

	METHOD ScrollDown(nLines AS LONG) AS VOID
		LOCAL currentIndex	AS LONG
		LOCAL nRecno		AS LONG
		LOCAL oRow			AS VODataGridViewRow
		currentIndex := _oGridView:TopRowIndex
		IF currentIndex < _oGridView:Rows:Count - nLines
			_oGridView:TopRowIndex := currentIndex +  nLines
		ELSEIF _oBrowser:HasBottom
			// Do nothing. All the data is already there
            NOP

		ELSE
			// Load new rows after the last row in the buffer and then position
			oRow	 := (VODataGridViewRow) _oGridView:Rows[currentIndex]
			nRecno   := oRow:RecNo
			_oBrowser:__DeltaBuildBufferDown()
			oRow     := _oBrowser:__GetRowAtRecNo(nRecno)
			_oGridView:TopRowIndex := oRow:Index + nLines
		ENDIF
		SELF:SyncScrollBar()

	METHOD SyncScrollBar() AS VOID STRICT
		// This sets the Thumb Position
		LOCAL iCount AS LONG
		LOCAL iPos	 AS LONG
		LOCAL iMax	 AS LONG
		LOCAL iPageSize AS LONG
		SELF:SetRange()
		iCount		:= _oGridView:Rows:Count
		iPos		:= _oGridView:TopRowIndex
		iPageSize	:= _oGridView:VisibleRows
		IF iCount == 0 .or. iPos < 0
			iPos := _oScrollBar:Minimum
		ELSEIF _oBrowser:HasBottom .and. iPos + iPageSize >= iCount
			IF _oGridView:SelectedRows:Count > 0
				LOCAL oRow AS DataGridViewRow
				oRow := _oGridView:SelectedRows[0]
				iPos := oRow:Index+1
				iMax := _nEnd - _nStart			// 800, 900 or 1000, depending on conditions
				iPos := (iMax * iPos / iCount) + _nStart
			ELSE
				iPos := SELF:_oScrollBar:Maximum
			ENDIF
		ELSE
			iMax	:= _nEnd - _nStart			// 800, 900 or 1000, depending on conditions
			iPos := (iMax * iPos / iCount) + _nStart
		ENDIF
		_oScrollBar:Value := iPos
		RETURN

	METHOD SyncGrid (nValue as LONG) AS VOID STRICT
		LOCAL iCount AS LONG
		LOCAL iPos	 AS LONG
		LOCAL iMax	 AS LONG
		iCount		:= _oGridView:Rows:Count
		IF iCount == 0
			RETURN
		ENDIF
		SELF:SetRange()
		IF SELF:IsValueVirtual
			IF nValue < _nStart
				// Browser should read data before first line
				_oBrowser:__DeltaBuildBufferUp()
				iPos := -1
			ELSEIF nValue > _nEnd
				// Browser should read data before first line
				_oBrowser:__DeltaBuildBufferDown()
				iPos := -1
			ELSE
				iMax := _nEnd - _nStart
				iPos := _oScrollBar:Value * iCount / iMax
			ENDIF
		ELSE
			iMax := _oScrollBar:Maximum - _oScrollBar:Minimum
			iPos := nValue * iCount / iMax

		ENDIF
		IF iPos >= 0
			_oGridView:TopRowIndex := iPos
		ENDIF
		RETURN

END CLASS


#region defines
DEFINE GBSSBLEFT := 1
DEFINE GBSSBMIDDLE := 2
DEFINE GBSSBRIGHT := 3
DEFINE ssBlockSelection      := 3
DEFINE ssExtendedSelection := 2
DEFINE ssNoSelection         := 0
DEFINE ssSingleSelection     := 1
DEFINE __WCGBNotifyWindowClass := "GBNotifyContext"
#endregion
