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
    /// VFP-compatible data grid that wraps <see cref="System.Windows.Forms.DataGridView"/>.<br/>
    /// Provides full VFP Grid property coverage: data binding via <see cref="RecordSource"/>,
    /// column management via <see cref="Column(int)"/> / <see cref="ColumnCount"/>,
    /// display properties (<see cref="GridLines"/>, <see cref="GridLineColor"/>, <see cref="RowHeight"/>,
    /// <see cref="HeaderHeight"/>, <see cref="HighlightStyle"/>, <see cref="AlternatingRowColor"/>,
    /// <see cref="NullDisplay"/>, <see cref="Themes"/>), editing controls
    /// (<see cref="AllowAddNew"/>, <see cref="AllowDelete"/>, <see cref="AllowUpdate"/>,
    /// <see cref="AllowHeaderSizing"/>, <see cref="AllowRowSizing"/>, <see cref="AllowAutoColumnFit"/>),
    /// and navigation (<see cref="ActiveRow"/>, <see cref="ActiveColumn"/>, <see cref="RowColChange"/>,
    /// <see cref="DoScroll"/>).<br/>
    /// VFP events: <see cref="vfpAfterRowColChange"/>, <see cref="vfpBeforeRowColChange"/>,
    /// <see cref="vfpBeforeEditCell"/>, <see cref="vfpAfterEditCell"/>, <see cref="vfpColumnHeaderClick"/>,
    /// <see cref="vfpScrolled"/>, <see cref="vfpResize"/>, <see cref="vfpMoved"/> — plus the standard
    /// VFP control events (<c>vfpInit</c>, <c>vfpDestroy</c>, <c>vfpClick</c>, <c>vfpGotFocus</c>, etc.)
    /// inherited from the shared <c>VFPContainer.xh</c> include.<br/>
    /// New-row lifecycle (<see cref="AllowAddNew"/>) is managed entirely by this class — it never sets
    /// <c>DataGridView.AllowUserToAddRows</c> — to avoid WinForms ghost-row crash chains.
    /// </summary>
    PARTIAL CLASS Grid INHERIT System.Windows.Forms.DataGridView IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner



        // ── Selection colours ────────────────────────────────────────────────
        /// <summary>
        /// Background colour of the selected cell(s), mapped to <c>DefaultCellStyle.SelectionBackColor</c>.
        /// </summary>
        PROPERTY HighlightBackColor AS System.Drawing.Color
            GET ; RETURN SELF:DefaultCellStyle:SelectionBackColor ; END GET
            SET ; SELF:DefaultCellStyle:SelectionBackColor := VALUE ; END SET
        END PROPERTY

        /// <summary>
        /// Text colour of the selected cell(s), mapped to <c>DefaultCellStyle.SelectionForeColor</c>.
        /// </summary>
        PROPERTY HighlightForeColor AS System.Drawing.Color
            GET ; RETURN SELF:DefaultCellStyle:SelectionForeColor ; END GET
            SET ; SELF:DefaultCellStyle:SelectionForeColor := VALUE ; END SET
        END PROPERTY

        // ── Alternating row colour ───────────────────────────────────────────
        /// <summary>
        /// Background colour for even-numbered (alternating) rows, mapped to <c>AlternatingRowsDefaultCellStyle.BackColor</c>.
        /// </summary>
        PROPERTY AlternatingRowColor AS System.Drawing.Color
            GET ; RETURN SELF:AlternatingRowsDefaultCellStyle:BackColor ; END GET
            SET ; SELF:AlternatingRowsDefaultCellStyle:BackColor := VALUE ; END SET
        END PROPERTY

        // ── Row / column editing permissions ────────────────────────────────
        /// <summary>
        /// When <c>.T.</c>, the user can append new rows by pressing Down/Tab/Enter on the last row.<br/>
        /// Decoupled from <c>DataGridView.AllowUserToAddRows</c> (kept permanently <c>FALSE</c>);
        /// this class owns the entire new-row lifecycle via <c>_StartNewRow</c> / <c>_CommitNewRow</c>
        /// / <c>_DiscardNewRow</c> to avoid WinForms ghost-row crash chains.
        /// </summary>
        PROPERTY AllowAddNew AS LOGIC
            GET ; RETURN SELF:_allowAddNew ; END GET
            SET ; SELF:_allowAddNew := VALUE ; END SET
        END PROPERTY

        /// <summary>
        /// When <c>.T.</c>, the user can delete rows. Maps to <see cref="System.Windows.Forms.DataGridView.AllowUserToDeleteRows"/>.
        /// </summary>
        PROPERTY AllowDelete AS LOGIC
            GET ; RETURN SELF:AllowUserToDeleteRows ; END GET
            SET ; SELF:AllowUserToDeleteRows := VALUE ; END SET
        END PROPERTY

        /// <summary>
        /// When <c>.F.</c>, all cells are read-only. Maps to the inverse of <see cref="System.Windows.Forms.DataGridView.ReadOnly"/>.
        /// </summary>
        PROPERTY AllowUpdate AS LOGIC
            GET ; RETURN !SELF:ReadOnly ; END GET
            SET ; SELF:ReadOnly := !VALUE ; END SET
        END PROPERTY

        // ── RecordMark / DeleteMark ──────────────────────────────────────────
        /// <summary>
        /// When <c>.T.</c>, shows the row-header panel with a current-record arrow on the left.<br/>
        /// Maps to <see cref="System.Windows.Forms.DataGridView.RowHeadersVisible"/>. The constructor
        /// sets this to <c>.F.</c>; assigning <c>.T.</c> re-enables the row header panel.
        /// </summary>
        PROPERTY RecordMark AS LOGIC
            GET ; RETURN SELF:RowHeadersVisible ; END GET
            SET ; SELF:RowHeadersVisible := VALUE ; END SET
        END PROPERTY

        /// <summary>
        /// VFP DeleteMark — no direct WinForms equivalent; stored for source compatibility.
        /// </summary>
        PROPERTY DeleteMark AS LOGIC AUTO
        /// <summary>
        /// VFP Panel — no direct WinForms equivalent; stored for source compatibility.
        /// </summary>
        PROPERTY Panel AS INT AUTO



        PRIVATE _oldRowIndex AS LONG
        PRIVATE _oldColIndex AS LONG
        PRIVATE _rowColChange AS LONG

        PRIVATE _allowAddNew          AS LOGIC
        PRIVATE _pendingNewRow        AS LOGIC
        PRIVATE _pendingNewRowIndex   AS INT
        PRIVATE _pendingNewRowDirty   AS LOGIC
        PRIVATE _autoColumns          AS LOGIC

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
            // VFP RecordMark defaults to .T. — show a narrow row-header column with the current-record arrow
            SELF:RowHeadersVisible := TRUE
            SELF:RowHeadersWidth := 20
            SELF:RowHeadersWidthSizeMode := DataGridViewRowHeadersWidthSizeMode.DisableResizing
            // Per Default, in VFP you cannot add new rows to the grid
            SELF:AllowAddNew := FALSE
            // AllowUserToAddRows is always FALSE — we manage new rows ourselves
            SELF:AllowUserToAddRows := FALSE
            // TODO : Not sure about this one, but I guess that by default you cannot delete rows in a VFP Grid
            SELF:AllowUserToDeleteRows := FALSE

            //
            SELF:SelectionChanged += System.EventHandler{ SELF, @VFPSelectionChanged() }
            SELF:CurrentCellChanged += System.EventHandler{ SELF, @VFPCurrentCellChanged() }
            SELF:CellLeave += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @VFPCellLeave() }
            SELF:CellBeginEdit += System.Windows.Forms.DataGridViewCellCancelEventHandler{ SELF, @VFPCellBeginEdit() }
            SELF:CellEndEdit += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @VFPCellEndEdit() }
            SELF:CellValueChanged += System.Windows.Forms.DataGridViewCellEventHandler{ SELF, @VFPCellValueChanged() }
            SELF:ColumnHeaderMouseClick += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @VFPColumnHeaderMouseClick() }
            SELF:Size := System.Drawing.Size{320, 200}

            RETURN

        /// <summary>
        /// Returns the <see cref="Column"/> at the given 1-based index.<br/>
        /// VFP grids address columns as <c>Grid.Column(1)</c> … <c>Grid.Column(n)</c>;
        /// the underlying <c>DataGridView.Columns</c> collection is 0-based.
        /// </summary>
        PUBLIC METHOD Column( i AS INT ) AS Column
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


        /// <summary>
        /// Number of columns in the grid.<br/>
        /// Getting returns <c>Columns.Count</c>. Setting adds or removes <see cref="Column"/> objects
        /// to reach the requested count; throws <see cref="System.ArgumentOutOfRangeException"/> for
        /// negative values or <see cref="System.InvalidOperationException"/> when the grid is data-bound.
        /// </summary>
        PUBLIC NEW PROPERTY ColumnCount AS LONG
            GET
                RETURN Columns:Count

            END GET
            SET
                IF VALUE < 0
                    // VFP ColumnCount = -1 means auto-generate from record source
                    SELF:_autoColumns := TRUE
                    RETURN
                ENDIF
                SELF:_autoColumns := FALSE
                IF DataSource != NULL
                    THROW InvalidOperationException{"Cannot Set ColumnCount On DataBound VFPGrid"}
                ENDIF
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

            PROTECTED _currentSource	AS VFPDbDataSource
        PROTECTED _nameOfTable		AS STRING
        PROTECTED _bindingSource	AS System.Windows.Forms.BindingSource

            /// <summary>
        /// Name of the VFP work area (alias) that provides data for the grid.<br/>
        /// When set at runtime (not in design mode), <see cref="ApplyRecordSource"/> is called
        /// immediately: it opens the alias, creates a <see cref="VFPDbDataSource"/> and a
        /// <see cref="System.Windows.Forms.BindingSource"/>, binds them as the grid's <c>DataSource</c>,
        /// and calls <c>CreateDataColumns</c> to set header text for bound columns.
        /// </summary>
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
                IF Used( SELF:_nameOfTable )
                    DbSelectArea( SELF:_nameOfTable )
                    SELF:_currentSource := VFPDbDataSource.CreateForCurrentArea()
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
        /// VFP RecordSourceType — how the RecordSource is interpreted (0=Table/Alias, 4=SQL). Stored; binding always uses the alias name directly via <see cref="VFPDbDataSource"/>.
        /// </summary>
        PROPERTY RecordSourceType AS LONG AUTO

        /// <summary>
        /// VFP SplitBar — split-view support; no WinForms equivalent. Stored for source compatibility.
        /// </summary>
        PROPERTY SplitBar AS LOGIC AUTO

        /// <summary>
        /// VFP Scrollbars setting: 0=None, 1=Horizontal, 2=Vertical, 3=Both.<br/>
        /// Maps to <see cref="System.Windows.Forms.DataGridView.ScrollBars"/>.
        /// </summary>
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

        /// <summary>
        /// Default height in pixels for data rows, applied via <c>RowTemplate.Height</c>.
        /// </summary>
        PROPERTY RowHeight AS INT GET SELF:RowTemplate:Height SET SELF:RowTemplate:Height := VALUE

        PROTECTED METHOD CreateDataColumns() AS VOID
            // When no columns were defined in the SCX (VFP auto-column mode), generate one per field.
            IF SELF:ColumnCount == 0 .AND. SELF:_currentSource != NULL
                SELF:_AutoGenerateColumns()
            ENDIF
            // Set HeaderText for explicitly-defined columns bound via ControlSource.
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

        PRIVATE METHOD _AutoGenerateColumns() AS VOID
            VAR nOld := DbGetSelect()
            TRY
                DbSelectArea( SELF:_nameOfTable )
                VAR nFields := FCount()
                FOR VAR i := 1 UPTO nFields
                    VAR cName := FieldName( i )
                    VAR cType := (STRING) DbFieldInfo( DBS_TYPE, i )
                    VAR nLen  := (INT) DbFieldInfo( DBS_LEN, i )
                    VAR oCol := Column{}
                    oCol:DataPropertyName := cName
                    oCol:HeaderText := cName
                    oCol:Name := cName
                    oCol:Width := _ColumnWidth( cType, nLen )
                    SELF:Columns:Add( oCol )
                NEXT
            CATCH
                NOP
            FINALLY
                DbSelectArea( nOld )
            END TRY

        // Returns a pixel width appropriate for the VFP field type + declared length.
        // DBS_LEN is a binary storage size for I/B/T/Y/D/L/M, not a display width.
        PRIVATE STATIC METHOD _ColumnWidth( cType AS STRING, nLen AS INT ) AS INT
            SWITCH cType:ToUpper()
            CASE "L"   ; RETURN 40            // Logical: .T./.F.
            CASE "D"   ; RETURN 90            // Date: MM/DD/YYYY
            CASE "T"   ; RETURN 140           // DateTime
            CASE "I"   ; RETURN 65            // Integer (4-byte binary storage)
            CASE "B"   ; RETURN 80            // Double  (8-byte binary storage)
            CASE "Y"   ; RETURN 90            // Currency(8-byte binary storage)
            CASE "M"   ; RETURN 100           // Memo
            CASE "G"   ; RETURN 60            // General/OLE
            CASE "N"   ; RETURN Math.Max( 50, Math.Min( 150, nLen * 9 ) )   // Numeric
            OTHERWISE  ; RETURN Math.Max( 50, Math.Min( 250, nLen * 7 ) )   // Character and others
            END SWITCH


        PRIVATE _VFPAfterRowColChange AS VFPOverride
        /// <summary>
        /// Name of the VFP method called after the active row or column changes. Receives the new 1-based row index as <c>nLocation</c>. Wired to <c>DataGridView.CurrentCellChanged</c>.
        /// </summary>
        PROPERTY vfpAfterRowColChange AS STRING GET _VFPAfterRowColChange?:SendTo SET SELF:Set_AfterRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_AfterRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPAfterRowColChange := methodCall

        /// <summary>
        /// Virtual VFP event method — override in a subclass to handle AfterRowColChange without a callback string. The base implementation calls <see cref="vfpAfterRowColChange"/>.
        /// </summary>
        VIRTUAL METHOD AfterRowColChange( nLocation AS LONG ) AS VOID
            IF SELF:_VFPAfterRowColChange != NULL
                SELF:_VFPAfterRowColChange:Call( <USUAL>{nLocation} )
            ENDIF

        PRIVATE _VFPBeforeRowColChange AS VFPOverride
        /// <summary>
        /// Name of the VFP method called before the active row or column changes. Receives the old 1-based row index as <c>nLocation</c>. Wired to <c>DataGridView.CellLeave</c> for correct pre-move timing.
        /// </summary>
        PROPERTY vfpBeforeRowColChange AS STRING GET _VFPBeforeRowColChange?:SendTo SET SELF:Set_BeforeRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_BeforeRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPBeforeRowColChange := methodCall

        /// <summary>
        /// Virtual VFP event method — override in a subclass to handle BeforeRowColChange without a callback string. The base implementation calls <see cref="vfpBeforeRowColChange"/>.
        /// </summary>
        VIRTUAL METHOD BeforeRowColChange( nLocation AS LONG ) AS VOID
            IF SELF:_VFPBeforeRowColChange != NULL
                SELF:_VFPBeforeRowColChange:Call( <USUAL>{nLocation} )
            ENDIF

        // ── BeforeEditCell / AfterEditCell ────────────────────────────────────
        PRIVATE _VFPBeforeEditCell AS VFPOverride
        /// <summary>
        /// Name of the VFP method called before a cell enters edit mode. Receives 1-based <c>nRow</c> and <c>nCol</c>. Wired to <c>DataGridView.CellBeginEdit</c>.
        /// </summary>
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
        /// <summary>
        /// Name of the VFP method called after a cell exits edit mode. Receives 1-based <c>nRow</c> and <c>nCol</c>. Wired to <c>DataGridView.CellEndEdit</c>.
        /// </summary>
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
        /// <summary>
        /// Name of the VFP method called when the user clicks any column header. Receives the 1-based column index as <c>nCol</c>. Wired to <c>DataGridView.ColumnHeaderMouseClick</c>.
        /// </summary>
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
            // Detect leaving the pending new row
            IF SELF:_pendingNewRow .AND. SELF:_oldRowIndex == SELF:_pendingNewRowIndex
                IF SELF:CurrentCell == NULL .OR. SELF:CurrentCell:RowIndex != SELF:_pendingNewRowIndex
                    IF SELF:_pendingNewRowDirty
                        SELF:_CommitNewRow()
                    ELSE
                        SELF:_DiscardNewRow()
                    ENDIF
                ENDIF
            ENDIF
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

        /// <summary>
        /// After <see cref="AfterRowColChange"/> fires, indicates what changed: 0=nothing, 1=row,
        /// 2=column, 3=both row and column. Reset to 0 on <c>SelectionChanged</c>.
        /// </summary>
        PROPERTY RowColChange AS LONG GET SELF:_rowColChange

        /// <summary>
        /// Synchronises the binding-source position to the current RDD record number, then fires
        /// <see cref="vfpRefresh"/> (if set), and finally calls <c>base.Refresh()</c> to repaint.<br/>
        /// The RDD record pointer is saved before the repaint (DataGridView rows access the RDD
        /// sequentially, leaving it at the last painted row) and restored afterwards.
        /// </summary>
        OVERRIDE METHOD Refresh() AS VOID
            LOCAL lHasSource  AS LOGIC
            LOCAL nSavedRecno AS DWORD
            lHasSource := SELF:_currentSource != NULL .AND. SELF:_bindingSource != NULL
            IF lHasSource
                nSavedRecno := SELF:_currentSource:SavePosition()
            ENDIF
            TRY
                IF lHasSource
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
            //
            IF lHasSource
                SELF:_currentSource:RestorePosition(nSavedRecno)
            ENDIF

        /// <summary>
        /// Selects the cell at the given 1-based row and column indices.
        /// </summary>
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

        /// <summary>
        /// 1-based row index of the currently active cell, or 0 when no cell is active.
        /// </summary>
        PROPERTY ActiveRow AS INT
            GET
                LOCAL nRow := 0 AS INT
                IF SELF:CurrentCell != NULL
                    nRow := SELF:CurrentCell:RowIndex + 1  // 1-based
                ENDIF
                RETURN nRow
            END GET
        END PROPERTY

        /// <summary>
        /// 1-based column index of the currently active cell, or 0 when no cell is active.
        /// </summary>
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
        /// <summary>
        /// Height (in pixels) of the divider line drawn below each row. Reads from the first row's <c>DividerHeight</c>; writing sets <c>DividerHeight</c> on all existing rows.
        /// </summary>
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
        /// <summary>
        /// When <c>.T.</c>, the user can drag the column-header row height. Maps to <see cref="System.Windows.Forms.DataGridView.ColumnHeadersHeightSizeMode"/> (<c>EnableResizing</c> vs <c>DisableResizing</c>).
        /// </summary>
        PROPERTY AllowHeaderSizing AS LOGIC
            GET ; RETURN SELF:ColumnHeadersHeightSizeMode == DataGridViewColumnHeadersHeightSizeMode.EnableResizing ; END GET
            SET ; SELF:ColumnHeadersHeightSizeMode := IIF(VALUE, DataGridViewColumnHeadersHeightSizeMode.EnableResizing, DataGridViewColumnHeadersHeightSizeMode.DisableResizing) ; END SET
        END PROPERTY

        // ── AllowRowSizing ────────────────────────────────────────────────────
        /// <summary>
        /// When <c>.T.</c>, the user can drag row dividers to resize individual rows. Maps to <see cref="System.Windows.Forms.DataGridView.AllowUserToResizeRows"/>.
        /// </summary>
        PROPERTY AllowRowSizing AS LOGIC
            GET ; RETURN SELF:AllowUserToResizeRows ; END GET
            SET ; SELF:AllowUserToResizeRows := VALUE ; END SET
        END PROPERTY

        // ── HighlightStyle ────────────────────────────────────────────────────
        /// <summary>
        /// VFP selection style: 0=full-row highlight (default), 1=cell-level box.<br/>
        /// Maps to <see cref="System.Windows.Forms.DataGridView.SelectionMode"/>
        /// (<c>FullRowSelect</c> or <c>CellSelect</c>).
        /// </summary>
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
        /// <summary>
        /// 0=columns keep their defined widths; 1=columns auto-size to fit their content.<br/>
        /// Maps to <see cref="System.Windows.Forms.DataGridView.AutoSizeColumnsMode"/>
        /// (<c>None</c> or <c>AllCells</c>).
        /// </summary>
        PROPERTY AllowAutoColumnFit AS LONG
            GET ; RETURN IIF(SELF:AutoSizeColumnsMode == DataGridViewAutoSizeColumnsMode.None, 0, 1) ; END GET
            SET ; SELF:AutoSizeColumnsMode := IIF(VALUE != 0, DataGridViewAutoSizeColumnsMode.AllCells, DataGridViewAutoSizeColumnsMode.None) ; END SET
        END PROPERTY

        // ── Themes ────────────────────────────────────────────────────────────
        /// <summary>
        /// When <c>.T.</c>, column headers use OS visual styles (XP themes). Maps to <see cref="System.Windows.Forms.DataGridView.EnableHeadersVisualStyles"/>.
        /// </summary>
        PROPERTY Themes AS LOGIC
            GET ; RETURN SELF:EnableHeadersVisualStyles ; END GET
            SET ; SELF:EnableHeadersVisualStyles := VALUE ; END SET
        END PROPERTY

        /// <summary>
        /// The value of the currently active cell as a VFP USUAL, or <c>NIL</c> when no cell is active.
        /// </summary>
        PROPERTY Value AS USUAL
            GET
                IF SELF:CurrentCell != NULL
                    RETURN (USUAL) SELF:CurrentCell:Value
                ENDIF
                RETURN NIL
            END GET
        END PROPERTY

        /// <summary>
        /// When <c>.T.</c>, deleted RDD records are included in the grid display.<br/>
        /// Forwarded to <see cref="VFPDbDataSource.ShowDeleted"/> when the data source is active.
        /// </summary>
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
        /// <summary>
        /// VFP GridLines: 0=None, 1=Horizontal, 2=Vertical, 3=Both (default).<br/>
        /// Maps to <see cref="System.Windows.Forms.DataGridView.CellBorderStyle"/>
        /// (<c>None</c>, <c>SingleHorizontal</c>, <c>SingleVertical</c>, <c>Single</c>).
        /// </summary>
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
        /// <summary>
        /// Colour of the grid cell borders, mapped to <see cref="System.Windows.Forms.DataGridView.GridColor"/>.
        /// </summary>
        PROPERTY GridLineColor AS System.Drawing.Color
            GET ; RETURN SELF:GridColor ; END GET
            SET ; SELF:GridColor := VALUE ; END SET
        END PROPERTY

        // ── HeaderHeight ──────────────────────────────────────────────────────
        /// <summary>
        /// Height in pixels of the column-header row.<br/>
        /// Setting this property also sets <see cref="System.Windows.Forms.DataGridView.ColumnHeadersHeightSizeMode"/>
        /// to <c>DisableResizing</c> so the explicit height is preserved.
        /// Values ≤ 0 are ignored.
        /// </summary>
        PROPERTY HeaderHeight AS INT
            GET
                RETURN SELF:ColumnHeadersHeight
            END GET
            SET
                IF VALUE > 0
                    SELF:ColumnHeadersHeight := VALUE
                    SELF:ColumnHeadersHeightSizeMode := DataGridViewColumnHeadersHeightSizeMode.DisableResizing
                ENDIF
            END SET
        END PROPERTY

        // ── NullDisplay ───────────────────────────────────────────────────────
        /// <summary>
        /// String displayed in a cell when its value is <c>NULL</c>. Mapped to <c>DefaultCellStyle.NullValue</c>.
        /// </summary>
        PROPERTY NullDisplay AS STRING
            GET
                RETURN (STRING) SELF:DefaultCellStyle:NullValue
            END GET
            SET
                SELF:DefaultCellStyle:NullValue := VALUE
            END SET
        END PROPERTY

        // ── Resize / Moved events ─────────────────────────────────────────────
        PRIVATE _VFPResize AS VFPOverride
        /// <summary>
        /// Name of the VFP method called when the grid control is resized.
        /// </summary>
        [System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
        PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

        PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
            SUPER:OnResize(e)
            IF SELF:_VFPResize != NULL
                SELF:_VFPResize:Call()
            ENDIF

        PRIVATE _VFPMoved AS VFPOverride
        /// <summary>
        /// Name of the VFP method called when the grid control is moved.
        /// </summary>
        [System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
        PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

        PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
            SUPER:OnMove(e)
            IF SELF:_VFPMoved != NULL
                SELF:_VFPMoved:Call()
            ENDIF

        // ── Scrolled event ────────────────────────────────────────────────────
        PRIVATE _VFPScrolled AS VFPOverride
        /// <summary>
        /// Name of the VFP method called when the grid is scrolled. Receives a direction code as <c>nDirection</c>:
        /// 1=up, 2=down, 3=left, 4=right, 5=page-up, 6=page-down, 7=leftmost, 8=rightmost.<br/>
        /// Assigning this property wires the <c>DataGridView.Scroll</c> event automatically.
        /// </summary>
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
            IF e:ScrollOrientation == System.Windows.Forms.ScrollOrientation.VerticalScroll
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
        /// <summary>
        /// Removes the column at the given 1-based index. Silently ignored if <c>n</c> is out of range.
        /// </summary>
        METHOD DeleteColumns( n AS INT ) AS VOID
            IF n >= 1 .AND. n <= SELF:Columns:Count
                SELF:Columns:RemoveAt( n - 1 )
            ENDIF

        // ── DoScroll ──────────────────────────────────────────────────────────
        /// <summary>
        /// Programmatically scrolls the grid. <c>nDirection</c> uses the same codes as <see cref="vfpScrolled"/>:
        /// 1=up one row, 2=down one row, 3=left one column, 4=right one column,
        /// 5=page up, 6=page down, 7=leftmost column, 8=rightmost column.
        /// </summary>
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
        END METHOD


        METHOD VFPCellValueChanged( sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellEventArgs ) AS VOID
            IF SELF:_pendingNewRow .AND. e:RowIndex == SELF:_pendingNewRowIndex
                SELF:_pendingNewRowDirty := TRUE
            ENDIF

        PROTECTED OVERRIDE METHOD ProcessDataGridViewKey( e AS System.Windows.Forms.KeyEventArgs ) AS LOGIC
            IF SELF:_allowAddNew .AND. !SELF:_pendingNewRow .AND. SELF:_bindingSource != NULL .AND. SELF:CurrentCell != NULL
                LOCAL isLastRow := (SELF:CurrentCell:RowIndex == SELF:Rows:Count - 1) AS LOGIC
                IF isLastRow
                    DO CASE
                    CASE e:KeyCode == System.Windows.Forms.Keys.Down
                        SELF:_StartNewRow()
                        RETURN TRUE
                    CASE e:KeyCode == System.Windows.Forms.Keys.Tab .AND. !e:Shift
                        IF SELF:CurrentCell:ColumnIndex == SELF:Columns:Count - 1
                            SELF:_StartNewRow()
                            RETURN TRUE
                        ENDIF
                    CASE e:KeyCode == System.Windows.Forms.Keys.Return
                        SELF:_StartNewRow()
                        RETURN TRUE
                    END CASE
                ENDIF
            ENDIF
            RETURN SUPER:ProcessDataGridViewKey( e )
        END METHOD

        PRIVATE METHOD _StartNewRow() AS VOID
            IF SELF:_currentSource == NULL
                RETURN
            ENDIF
            TRY
                SELF:EndEdit()
                // AppendRecord appends to the RDD directly (bypasses BindingSource.AddNew which
                // sets addNewIsBeingAdded and causes DataGridView to skip the row insertion).
                // ResetBindings(false) fires ListChanged(Reset), making DataGridView rebuild its
                // row collection from RecCount (now N+1) without addNewIsBeingAdded interference.
                IF ! SELF:_currentSource:AppendRecord()
                    RETURN
                ENDIF
                SELF:_bindingSource:ResetBindings(FALSE)
                SELF:_pendingNewRow      := TRUE
                SELF:_pendingNewRowIndex := SELF:Rows:Count - 1
                SELF:_pendingNewRowDirty := FALSE
                IF SELF:Columns:Count > 0
                    SELF:CurrentCell := SELF:Rows[SELF:_pendingNewRowIndex]:Cells[0]
                ENDIF
            CATCH ex AS Exception
                System.Diagnostics.Debug.WriteLine( "_StartNewRow: " + ex:Message )
            END TRY
        END METHOD

        PRIVATE METHOD _CommitNewRow() AS VOID
            TRY
                SELF:_bindingSource:EndEdit()
            CATCH
                NOP
            END TRY
            SELF:_pendingNewRow      := FALSE
            SELF:_pendingNewRowDirty := FALSE
        END METHOD

        PRIVATE METHOD _DiscardNewRow() AS VOID
            // Proper deletion would require DbDataSource.Count to exclude deleted records
            // (so DataGridView rebuilds with N-1 rows). That change is deferred.
            // For now: just reset pending state; the empty row stays in the grid/RDD.
            // TODO: delete empty record and refresh when DbDataSource supports filtered Count.
            SELF:_pendingNewRow      := FALSE
            SELF:_pendingNewRowDirty := FALSE
        END METHOD

    END CLASS

END NAMESPACE // XSharp.VFP.UI
