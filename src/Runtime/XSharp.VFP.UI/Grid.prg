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

        /// <summary>
        /// Internal CurrentCellChanged. Will root to AfterRowColChange event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        METHOD VFPCurrentCellChanged(sender AS OBJECT, e AS System.EventArgs) AS VOID
            LOCAL nRowColIndex AS LONG
            // Same Row ? So, Column have changed
            IF SELF:CurrentCell != NULL
                IF SELF:_oldRowIndex != SELF:CurrentCell:RowIndex
                    SELF:_rowColChange := 1 // Row
                    nRowColIndex := SELF:_oldRowIndex
                ENDIF
                IF SELF:_oldColIndex != SELF:CurrentCell:ColumnIndex
                    SELF:_rowColChange += 2 // == 2 Col only; ==3 Both
                    nRowColIndex := SELF:_oldColIndex
                ENDIF
            ENDIF
            //
            SELF:BeforeRowColChange( nRowColIndex )
            SELF:AfterRowColChange( nRowColIndex )

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

        PROPERTY AllowHeaderSizing AS LOGIC AUTO
        PROPERTY AllowRowSizing AS LOGIC AUTO
        //OVERRIDE PROPERTY ReadOnly AS LOGIC AUTO
        PROPERTY HighlightStyle AS LONG AUTO
        PROPERTY AllowAutoColumnFit AS LONG AUTO
        PROPERTY Themes AS LOGIC AUTO

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

    END CLASS
END NAMESPACE // XSharp.VFP.UI
