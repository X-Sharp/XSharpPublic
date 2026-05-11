// Grid.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING XSharp.VFP
USING XSharp.RT

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// The VFP compatible Grid class.
    /// </summary>
    PARTIAL CLASS Grid INHERIT System.Windows.Forms.DataGridView IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner



        // ── Selection colours ────────────────────────────────────────────────
        // VFP HighlightBackColor/ForeColor → DataGridView selection cell style
        PROPERTY HighlightBackColor AS System.Drawing.Color
            GET ; RETURN SELF:DefaultCellStyle:SelectionBackColor ; END GET
            SET ; SELF:DefaultCellStyle:SelectionBackColor := VALUE ; END SET
        END PROPERTY

        PROPERTY HighlightForeColor AS System.Drawing.Color
            GET ; RETURN SELF:DefaultCellStyle:SelectionForeColor ; END GET
            SET ; SELF:DefaultCellStyle:SelectionForeColor := VALUE ; END SET
        END PROPERTY

        // ── Alternating row colour ───────────────────────────────────────────
        PROPERTY AlternatingRowColor AS System.Drawing.Color
            GET ; RETURN SELF:AlternatingRowsDefaultCellStyle:BackColor ; END GET
            SET ; SELF:AlternatingRowsDefaultCellStyle:BackColor := VALUE ; END SET
        END PROPERTY

        // ── Row / column editing permissions ────────────────────────────────
        PROPERTY AllowAddNew AS LOGIC
            GET ; RETURN SELF:AllowUserToAddRows ; END GET
            SET ; SELF:AllowUserToAddRows := VALUE ; END SET
        END PROPERTY

        PROPERTY AllowDelete AS LOGIC
            GET ; RETURN SELF:AllowUserToDeleteRows ; END GET
            SET ; SELF:AllowUserToDeleteRows := VALUE ; END SET
        END PROPERTY

        PROPERTY AllowUpdate AS LOGIC
            GET ; RETURN !SELF:ReadOnly ; END GET
            SET ; SELF:ReadOnly := !VALUE ; END SET
        END PROPERTY

        // ── RecordMark / DeleteMark ──────────────────────────────────────────
        // VFP RecordMark: shows a row-selector arrow on the left of the current record.
        // Maps to DataGridView.RowHeadersVisible (.T. = show, .F. = hide).
        // Note: Grid constructor sets RowHeadersVisible := FALSE; setting RecordMark := .T.
        // re-enables the row header panel.
        PROPERTY RecordMark AS LOGIC
            GET ; RETURN SELF:RowHeadersVisible ; END GET
            SET ; SELF:RowHeadersVisible := VALUE ; END SET
        END PROPERTY

        // VFP DeleteMark: shows a delete-mark column. No direct WinForms equivalent — stored only.
        PROPERTY DeleteMark AS LOGIC AUTO
        PROPERTY Panel AS INT AUTO



        PRIVATE _oldRowIndex AS LONG
        PRIVATE _oldColIndex AS LONG
        PRIVATE _rowColChange AS LONG

        #include "Headers/VFPContainer.xh"
        #include "Headers/VFPPropertiesDynamic.xh"
        #include "VFPProperties.xh"

        #include "ControlProperties.xh"


        CONSTRUCTOR(  ) STRICT
            SUPER()
            // Force a minimum value to the Row Template
            SELF:RowTemplate:Height := 24
            // We will manage the process
            SELF:AutoGenerateColumns := FALSE
            // Remove the "selection" column in front of each Row
            SELF:RowHeadersVisible := FALSE
            //
            SELF:SelectionChanged += System.EventHandler{ SELF, @VFPSelectionChanged() }
            SELF:CurrentCellChanged += System.EventHandler{ SELF, @VFPCurrentCellChanged() }
            SELF:CellLeave += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @VFPCellLeave() }
            SELF:CellBeginEdit += System.Windows.Forms.DataGridViewCellCancelEventHandler{ SELF, @VFPCellBeginEdit() }
            SELF:CellEndEdit += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @VFPCellEndEdit() }
            SELF:ColumnHeaderMouseClick += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @VFPColumnHeaderMouseClick() }
            SELF:Size := System.Drawing.Size{320, 200}

            RETURN

        PUBLIC METHOD Column( i AS INT ) AS Column
            // Looking for a Column
            //			IF i > ColumnCount
            //				// Asking for an non-existing Column ?
            //				// Add Columns up to i
            //				SELF:ColumnCount := i
            //			ENDIF
            // Warning, i is one-based; but the columns collection is zero based
            RETURN (Column)SELF:Columns[ i-1 ]


            // Todo
        PROPERTY FontSize AS Single
            GET
                VAR ft := SELF:Font
                RETURN ft:Size
            END GET
            SET
                VAR ft := SELF:Font
                VAR newFont := System.Drawing.Font{ ft.Name, (Single)VALUE, ft.Style, ft.Unit}
                SELF:Font := newFont
            END SET
        END PROPERTY


        // Override ColumnCount
        PUBLIC NEW PROPERTY ColumnCount AS LONG
            GET
                RETURN Columns:Count

            END GET
            SET
                //
                IF VALUE < 0
                    THROW ArgumentOutOfRangeException{"ColumnCount" }
                ENDIF
                IF DataSource != NULL
                    THROW InvalidOperationException{"Cannot Set ColumnCount On DataBound VFPGrid"}
                ENDIF
                //
                SELF:_SetColumnCount( VALUE )
            END SET
        END PROPERTY


        PRIVATE METHOD _SetColumnCount( columnMax AS LONG ) AS VOID
            LOCAL count AS LONG
            LOCAL count2 AS LONG
            //
            IF columnMax == Columns:Count
                RETURN
            ENDIF
            IF columnMax == 0
                Columns:Clear()
                RETURN
            ENDIF
            IF columnMax < Columns:Count
                WHILE columnMax < Columns:Count
                    count := Columns:Count
                    Columns:RemoveAt(count - 1)
                    IF Columns:Count >= count
                        EXIT
                    ENDIF
                END WHILE
                RETURN
            ENDIF
            WHILE columnMax > Columns:Count
                count2 := Columns:Count
                Columns:Add( Column{} )
                IF Columns:Count <= count2
                    EXIT
                ENDIF
            END WHILE

            PROTECTED _currentSource	AS DbDataSource
        PROTECTED _nameOfTable		AS STRING
        PROTECTED _bindingSource	AS System.Windows.Forms.BindingSource

            // This is the source of data to which the Grid control is bound
        PROPERTY RecordSource AS STRING
            SET
                IF !String.IsNullOrEmpty( VALUE )
                    SELF:_nameOfTable := VALUE
                    IF !SELF:DesignMode
                        SELF:ApplyRecordSource()
                    ENDIF
                ENDIF
            END SET
            GET
                RETURN IIF( String.IsNullOrEmpty(SELF:_nameOfTable), String.Empty, SELF:_nameOfTable )
            END GET
        END PROPERTY

        METHOD ApplyRecordSource() AS VOID
            IF String.IsNullOrEmpty(SELF:_nameOfTable) .OR. SELF:_currentSource != NULL
                RETURN
            ENDIF
            TRY
                VAR current := DbGetSelect()
                IF DbSelectArea( SELF:_nameOfTable )
                    SELF:_currentSource := DbDataSource()
                    IF SELF:_currentSource != NULL
                        SELF:_currentSource:ShowDeleted := SELF:_showDeleted
                        SELF:_currentSource:ShowRecno := FALSE
                        DbSelectArea( current )
                        SELF:_bindingSource := System.Windows.Forms.BindingSource{}
                        SELF:_bindingSource:DataSource := SELF:_currentSource
                        SELF:DataSource := SELF:_bindingSource
                        SELF:CreateDataColumns()
                    ENDIF
                ENDIF
            CATCH
                NOP
            END TRY

        /// <summary>
        ///
        /// </summary>
        /// <value></value>
        PROPERTY RecordSourceType AS LONG AUTO


            // Todo
        PROPERTY SplitBar AS LOGIC AUTO

            //
        PROPERTY VFPScrollbars AS LONG
            GET
                LOCAL _set := 0 AS LONG
                SWITCH SELF:ScrollBars
                    CASE System.Windows.Forms.ScrollBars.Horizontal
                        _set := 1
                    CASE System.Windows.Forms.ScrollBars.Vertical
                        _set := 2
                    CASE System.Windows.Forms.ScrollBars.Both
                        _set := 3
                END SWITCH
                RETURN _set
            END GET
            SET
                SWITCH VALUE
                    CASE 0
                        SELF:ScrollBars := System.Windows.Forms.ScrollBars.None
                    CASE 1
                        SELF:ScrollBars := System.Windows.Forms.ScrollBars.Horizontal
                    CASE 2
                        SELF:ScrollBars := System.Windows.Forms.ScrollBars.Vertical
                    CASE 3
                        SELF:ScrollBars := System.Windows.Forms.ScrollBars.Both
                END SWITCH
            END SET
        END PROPERTY

        // Todo
        PROPERTY RowHeight AS INT GET SELF:RowTemplate:Height SET SELF:RowTemplate:Height := VALUE

        PROTECTED METHOD CreateDataColumns() AS VOID
            // Set HeaderText for columns that have DataPropertyName bound via ControlSource.
            // Never auto-generate columns — VFP grids always have explicit column definitions.
            LOCAL f AS INT
            FOR f := 1 UPTO SELF:ColumnCount
                LOCAL oCol AS Column
                oCol := (Column)SELF:Columns[ f-1 ]
                IF !String.IsNullOrEmpty( oCol:DataPropertyName )
                    IF String.IsNullOrEmpty( oCol:HeaderText ) .OR. oCol:HeaderText == oCol:Name
                        oCol:HeaderText := oCol:DataPropertyName
                    ENDIF
                ENDIF
            NEXT


        PRIVATE _VFPAfterRowColChange AS VFPOverride
        PROPERTY vfpAfterRowColChange AS STRING GET _VFPAfterRowColChange?:SendTo SET SELF:Set_AfterRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_AfterRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPAfterRowColChange := methodCall

        // Overrideable VFP event method — subclasses override this
        VIRTUAL METHOD AfterRowColChange( nLocation AS LONG ) AS VOID
            IF SELF:_VFPAfterRowColChange != NULL
                SELF:_VFPAfterRowColChange:Call( <USUAL>{nLocation} )
            ENDIF

        PRIVATE _VFPBeforeRowColChange AS VFPOverride
        PROPERTY vfpBeforeRowColChange AS STRING GET _VFPBeforeRowColChange?:SendTo SET SELF:Set_BeforeRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_BeforeRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPBeforeRowColChange := methodCall

        // Overrideable VFP event method — subclasses override this
        VIRTUAL METHOD BeforeRowColChange( nLocation AS LONG ) AS VOID
            IF SELF:_VFPBeforeRowColChange != NULL
                SELF:_VFPBeforeRowColChange:Call( <USUAL>{nLocation} )
            ENDIF

        // ── BeforeEditCell / AfterEditCell ────────────────────────────────────
        PRIVATE _VFPBeforeEditCell AS VFPOverride
        PROPERTY vfpBeforeEditCell AS STRING GET _VFPBeforeEditCell?:SendTo SET SELF:Set_BeforeEditCell( VFPOverride{SELF, VALUE} )

        METHOD Set_BeforeEditCell( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPBeforeEditCell := methodCall

        VIRTUAL METHOD BeforeEditCell( nRow AS LONG, nCol AS LONG ) AS VOID
            IF SELF:_VFPBeforeEditCell != NULL
                SELF:_VFPBeforeEditCell:Call( <USUAL>{nRow, nCol} )
            ENDIF

        METHOD VFPCellBeginEdit( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellCancelEventArgs ) AS VOID
            SELF:BeforeEditCell( e:RowIndex + 1, e:ColumnIndex + 1 )

        PRIVATE _VFPAfterEditCell AS VFPOverride
        PROPERTY vfpAfterEditCell AS STRING GET _VFPAfterEditCell?:SendTo SET SELF:Set_AfterEditCell( VFPOverride{SELF, VALUE} )

        METHOD Set_AfterEditCell( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPAfterEditCell := methodCall

        VIRTUAL METHOD AfterEditCell( nRow AS LONG, nCol AS LONG ) AS VOID
            IF SELF:_VFPAfterEditCell != NULL
                SELF:_VFPAfterEditCell:Call( <USUAL>{nRow, nCol} )
            ENDIF

        METHOD VFPCellEndEdit( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs ) AS VOID
            SELF:AfterEditCell( e:RowIndex + 1, e:ColumnIndex + 1 )

        // ── OnColumnHeaderClick ───────────────────────────────────────────────
        PRIVATE _VFPColumnHeaderClick AS VFPOverride
        PROPERTY vfpColumnHeaderClick AS STRING GET _VFPColumnHeaderClick?:SendTo SET SELF:Set_ColumnHeaderClick( VFPOverride{SELF, VALUE} )

        METHOD Set_ColumnHeaderClick( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPColumnHeaderClick := methodCall

        VIRTUAL METHOD ColumnHeaderClick( nCol AS LONG ) AS VOID
            IF SELF:_VFPColumnHeaderClick != NULL
                SELF:_VFPColumnHeaderClick:Call( <USUAL>{nCol} )
            ENDIF

        METHOD VFPColumnHeaderMouseClick( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs ) AS VOID
            SELF:ColumnHeaderClick( e:ColumnIndex + 1 )

        // CellLeave fires before focus moves to the next cell — correct timing for BeforeRowColChange.
        // e.RowIndex / e.ColumnIndex are the cell being LEFT (VFP nLocation = old position).
        METHOD VFPCellLeave( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs ) AS VOID
            SELF:BeforeRowColChange( e:RowIndex + 1 )

        /// <summary>
        /// Internal CurrentCellChanged. Fires AfterRowColChange with the NEW cell position.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        METHOD VFPCurrentCellChanged(sender AS OBJECT, e AS System.EventArgs) AS VOID
            // Track what changed (row, col, or both) for the RowColChange property.
            IF SELF:CurrentCell != NULL
                IF SELF:_oldRowIndex != SELF:CurrentCell:RowIndex
                    SELF:_rowColChange := 1 // Row
                ENDIF
                IF SELF:_oldColIndex != SELF:CurrentCell:ColumnIndex
                    SELF:_rowColChange += 2 // == 2 Col only; == 3 Both
                ENDIF
                // AfterRowColChange receives the NEW location (1-based row index).
                SELF:AfterRowColChange( SELF:CurrentCell:RowIndex + 1 )
            ENDIF

        /// <summary>
        /// Internal SelectionChanged, use to get "old" Row & Col
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        METHOD VFPSelectionChanged(sender AS OBJECT, e AS System.EventArgs) AS VOID
            //
            IF SELF:CurrentCell != NULL
                SELF:_oldRowIndex  := SELF:CurrentCell:RowIndex
                SELF:_oldColIndex  := SELF:CurrentCell:ColumnIndex
                SELF:_rowColChange := 0
            ELSE
                SELF:_oldRowIndex  := 0
                SELF:_oldColIndex  := 0
                SELF:_rowColChange := 0
            ENDIF

        PROPERTY RowColChange AS LONG GET SELF:_rowColChange

        OVERRIDE METHOD Refresh() AS VOID
            TRY
                IF SELF:_currentSource != NULL .AND. SELF:_bindingSource != NULL
                    SELF:_bindingSource:Position := (INT) SELF:_currentSource:RecNo
                ENDIF
            CATCH
                NOP
            END TRY
            //
            IF SELF:_VFPRefresh != NULL .AND. !SELF:_vfpRefresh:InCall
                SELF:_VFPRefresh:Call()
            ENDIF
            //
            SUPER:Refresh()

        METHOD ActivateCell( nRow AS INT, nCol AS INT ) AS VOID
            //
            IF ( nRow <= SELF:Rows:Count )
                //dgView.rows[0].cells[0].selected = true,
                VAR oneRow := SELF:Rows[ nRow - 1]
                IF ( nCol < oneRow:cells:Count )
                    oneRow:cells[ nCol - 1 ]:Selected := TRUE
                ENDIF
            ENDIF
        END METHOD

        PROPERTY ActiveRow AS INT
            GET
                LOCAL nRow := 0 AS INT
                IF SELF:CurrentCell != NULL
                    nRow := SELF:CurrentCell:RowIndex + 1  // 1-based
                ENDIF
                RETURN nRow
            END GET
        END PROPERTY

        PROPERTY ActiveColumn AS INT
            GET
                LOCAL nCol := 0 AS INT
                IF SELF:CurrentCell != NULL
                    nCol := SELF:CurrentCell:ColumnIndex + 1  // 1-based
                ENDIF
                RETURN nCol
            END GET
        END PROPERTY


        PRIVATE _gridLineWidth AS INT
        PROPERTY GridLineWidth AS INT
            GET
                // Let's say that the first row will give the info
                IF SELF:Rows:Count > 0
                    VAR firstRow := SELF:Rows[0]
                    _gridLineWidth := firstRow:DividerHeight
                ELSE
                    _gridLineWidth := 1
                ENDIF
                RETURN _gridLineWidth
            END GET

            SET
                IF SELF:Rows:Count > 0
                    FOREACH dRow AS DataGridViewRow IN SELF:Rows
                        dRow:DividerHeight := VALUE
                    NEXT
                ENDIF
            END SET
        END PROPERTY

        // ── AllowHeaderSizing ─────────────────────────────────────────────────
        // VFP: .T. = user can drag column header height. Maps to ColumnHeadersHeightSizeMode.
        PROPERTY AllowHeaderSizing AS LOGIC
            GET ; RETURN SELF:ColumnHeadersHeightSizeMode == DataGridViewColumnHeadersHeightSizeMode.EnableResizing ; END GET
            SET ; SELF:ColumnHeadersHeightSizeMode := IIF(VALUE, DataGridViewColumnHeadersHeightSizeMode.EnableResizing, DataGridViewColumnHeadersHeightSizeMode.DisableResizing) ; END SET
        END PROPERTY

        // ── AllowRowSizing ────────────────────────────────────────────────────
        // VFP: .T. = user can drag row dividers to resize rows.
        PROPERTY AllowRowSizing AS LOGIC
            GET ; RETURN SELF:AllowUserToResizeRows ; END GET
            SET ; SELF:AllowUserToResizeRows := VALUE ; END SET
        END PROPERTY

        // ── HighlightStyle ────────────────────────────────────────────────────
        // VFP: 0=Standard full-row, 1=Cell-level box. Maps to DataGridView.SelectionMode.
        PROPERTY HighlightStyle AS LONG
            GET
                SWITCH SELF:SelectionMode
                CASE DataGridViewSelectionMode.CellSelect ; RETURN 1
                OTHERWISE                                 ; RETURN 0
                END SWITCH
            END GET
            SET
                SWITCH VALUE
                CASE 1 ; SELF:SelectionMode := DataGridViewSelectionMode.CellSelect
                OTHERWISE ; SELF:SelectionMode := DataGridViewSelectionMode.FullRowSelect
                END SWITCH
            END SET
        END PROPERTY

        // ── AllowAutoColumnFit ────────────────────────────────────────────────
        // VFP: 0=None, 1=Auto-size all columns to content.
        PROPERTY AllowAutoColumnFit AS LONG
            GET ; RETURN IIF(SELF:AutoSizeColumnsMode == DataGridViewAutoSizeColumnsMode.None, 0, 1) ; END GET
            SET ; SELF:AutoSizeColumnsMode := IIF(VALUE != 0, DataGridViewAutoSizeColumnsMode.AllCells, DataGridViewAutoSizeColumnsMode.None) ; END SET
        END PROPERTY

        // ── Themes ────────────────────────────────────────────────────────────
        // VFP: .T. = use OS visual styles for column headers.
        PROPERTY Themes AS LOGIC
            GET ; RETURN SELF:EnableHeadersVisualStyles ; END GET
            SET ; SELF:EnableHeadersVisualStyles := VALUE ; END SET
        END PROPERTY

        // C-13: Value = the current cell's value
        PROPERTY Value AS USUAL
            GET
                IF SELF:CurrentCell != NULL
                    RETURN (USUAL) SELF:CurrentCell:Value
                ENDIF
                RETURN NIL
            END GET
        END PROPERTY

        // C-14: ShowDeleted — controls whether deleted records are visible in the DataSource
        PRIVATE _showDeleted AS LOGIC
        PROPERTY ShowDeleted AS LOGIC
            GET
                RETURN _showDeleted
            END GET
            SET
                _showDeleted := VALUE
                IF SELF:_currentSource != NULL
                    SELF:_currentSource:ShowDeleted := VALUE
                ENDIF
            END SET
        END PROPERTY

        // DataGridView does not subscribe to KeyPress automatically.
        // ControlEventHandlers.xh only stores the handler; we must dispatch it here.
        PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
            SUPER:OnKeyPress(e)
            SELF:OnVFPKeyPress(SELF, e)

        // ── GridLines ─────────────────────────────────────────────────────────
        // VFP: 0=None, 1=Horizontal, 2=Vertical, 3=Both (default)
        PROPERTY GridLines AS LONG
            GET
                SWITCH SELF:CellBorderStyle
                CASE DataGridViewCellBorderStyle.SingleHorizontal  ; RETURN 1
                CASE DataGridViewCellBorderStyle.SingleVertical    ; RETURN 2
                CASE DataGridViewCellBorderStyle.Single            ; RETURN 3
                OTHERWISE                                          ; RETURN 0
                END SWITCH
            END GET
            SET
                SWITCH VALUE
                CASE 0 ; SELF:CellBorderStyle := DataGridViewCellBorderStyle.None
                CASE 1 ; SELF:CellBorderStyle := DataGridViewCellBorderStyle.SingleHorizontal
                CASE 2 ; SELF:CellBorderStyle := DataGridViewCellBorderStyle.SingleVertical
                OTHERWISE ; SELF:CellBorderStyle := DataGridViewCellBorderStyle.Single
                END SWITCH
            END SET
        END PROPERTY

        // ── GridLineColor ─────────────────────────────────────────────────────
        PROPERTY GridLineColor AS System.Drawing.Color
            GET ; RETURN SELF:GridColor ; END GET
            SET ; SELF:GridColor := VALUE ; END SET
        END PROPERTY

        // ── HeaderHeight ──────────────────────────────────────────────────────
        PROPERTY HeaderHeight AS INT
            GET ; RETURN SELF:ColumnHeadersHeight ; END GET
            SET ; IF VALUE > 0 ; SELF:ColumnHeadersHeight := VALUE ; SELF:ColumnHeadersHeightSizeMode := DataGridViewColumnHeadersHeightSizeMode.DisableResizing ; ENDIF ; END SET
        END PROPERTY

        // ── NullDisplay ───────────────────────────────────────────────────────
        // VFP NullDisplay: string shown when cell value is NULL
        PROPERTY NullDisplay AS STRING
            GET ; RETURN (STRING) SELF:DefaultCellStyle:NullValue ; END GET
            SET ; SELF:DefaultCellStyle:NullValue := VALUE ; END SET
        END PROPERTY

        // ── Resize / Moved events ─────────────────────────────────────────────
        PRIVATE _VFPResize AS VFPOverride
        [System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
        PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

        PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
            SUPER:OnResize(e)
            IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

        PRIVATE _VFPMoved AS VFPOverride
        [System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
        PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

        PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
            SUPER:OnMove(e)
            IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

        // ── Scrolled event ────────────────────────────────────────────────────
        // VFP nDirection: 1=up, 2=down, 3=left, 4=right, 5=pageup, 6=pagedown, 7=leftmost, 8=rightmost
        PRIVATE _VFPScrolled AS VFPOverride
        [System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
        PROPERTY vfpScrolled AS STRING GET _VFPScrolled?:SendTo SET SELF:Set_Scrolled( VFPOverride{SELF, VALUE} )

        METHOD Set_Scrolled( methodCall AS VFPOverride ) AS VOID
            SELF:Scroll += System.Windows.Forms.ScrollEventHandler{ SELF, @VFPOnScroll() }
            SELF:_VFPScrolled := methodCall

        VIRTUAL METHOD Scrolled( nDirection AS LONG ) AS VOID
            IF SELF:_VFPScrolled != NULL
                SELF:_VFPScrolled:Call( <USUAL>{nDirection} )
            ENDIF

        PRIVATE METHOD VFPOnScroll( sender AS OBJECT, e AS System.Windows.Forms.ScrollEventArgs ) AS VOID
            LOCAL nDir AS LONG
            IF e:ScrollOrientation == System.Windows.Forms.ScrollOrientation.Vertical
                SWITCH e:Type
                CASE System.Windows.Forms.ScrollEventType.SmallDecrement  ; nDir := 1
                CASE System.Windows.Forms.ScrollEventType.SmallIncrement  ; nDir := 2
                CASE System.Windows.Forms.ScrollEventType.LargeDecrement  ; nDir := 5
                CASE System.Windows.Forms.ScrollEventType.LargeIncrement  ; nDir := 6
                OTHERWISE                                                   ; nDir := 0
                END SWITCH
            ELSE
                SWITCH e:Type
                CASE System.Windows.Forms.ScrollEventType.SmallDecrement  ; nDir := 3
                CASE System.Windows.Forms.ScrollEventType.SmallIncrement  ; nDir := 4
                CASE System.Windows.Forms.ScrollEventType.First           ; nDir := 7
                CASE System.Windows.Forms.ScrollEventType.Last            ; nDir := 8
                OTHERWISE                                                   ; nDir := 0
                END SWITCH
            ENDIF
            IF nDir != 0
                SELF:Scrolled( nDir )
            ENDIF

        // ── DeleteColumns ─────────────────────────────────────────────────────
        // VFP: oGrid.DeleteColumns(nColumnIndex)  — 1-based
        METHOD DeleteColumns( n AS INT ) AS VOID
            IF n >= 1 .AND. n <= SELF:Columns:Count
                SELF:Columns:RemoveAt( n - 1 )
            ENDIF

        // ── DoScroll ──────────────────────────────────────────────────────────
        // VFP nDirection: 1=up, 2=down, 3=left, 4=right, 5=pageup, 6=pagedown, 7=leftmost, 8=rightmost
        METHOD DoScroll( nDirection AS INT ) AS VOID
            SWITCH nDirection
            CASE 1 // up one row
                IF SELF:FirstDisplayedScrollingRowIndex > 0
                    SELF:FirstDisplayedScrollingRowIndex -= 1
                ENDIF
            CASE 2 // down one row
                IF SELF:FirstDisplayedScrollingRowIndex < SELF:RowCount - 1
                    SELF:FirstDisplayedScrollingRowIndex += 1
                ENDIF
            CASE 3 // left one column
                IF SELF:FirstDisplayedScrollingColumnIndex > 0
                    SELF:FirstDisplayedScrollingColumnIndex -= 1
                ENDIF
            CASE 4 // right one column
                IF SELF:FirstDisplayedScrollingColumnIndex < SELF:ColumnCount - 1
                    SELF:FirstDisplayedScrollingColumnIndex += 1
                ENDIF
            CASE 5 // page up
                VAR visRows := SELF:DisplayedRowCount(FALSE)
                SELF:FirstDisplayedScrollingRowIndex := Math.Max(0, SELF:FirstDisplayedScrollingRowIndex - visRows)
            CASE 6 // page down
                VAR visRows2 := SELF:DisplayedRowCount(FALSE)
                SELF:FirstDisplayedScrollingRowIndex := Math.Min(SELF:RowCount - 1, SELF:FirstDisplayedScrollingRowIndex + visRows2)
            CASE 7 // leftmost
                SELF:FirstDisplayedScrollingColumnIndex := 0
            CASE 8 // rightmost
                SELF:FirstDisplayedScrollingColumnIndex := SELF:ColumnCount - 1
            END SWITCH

    END CLASS
END NAMESPACE // XSharp.VFP.UI
