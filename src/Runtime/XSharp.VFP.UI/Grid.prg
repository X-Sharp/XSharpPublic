// VFPGrid.prg
// Created by    : fabri
// Creation Date : 5/4/2022 7:57:45 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING XSharp.VFP
USING XSharp.RT

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// The VFPGrid class.
    /// </summary>
    CLASS Grid INHERIT System.Windows.Forms.DataGridView IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner



        PROPERTY HighlightBackColor AS LONG AUTO
        PROPERTY HighlightForeColor AS LONG AUTO




        PRIVATE _oldRowIndex AS LONG
        PRIVATE _oldColIndex AS LONG
        PRIVATE _rowColChange AS LONG

        #include "Generated\VFPContainer.xh"
        #include "XSharp\VFPProperties.xh"

        #include ".\Headers\ControlProperties.xh"


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

            // Todo
            PROPERTY DeleteMark AS LOGIC AUTO
            // Todo
        PROPERTY RecordMark AS LOGIC AUTO

            // Todo
        PROPERTY Panel AS INT AUTO


        PROTECTED _currentSource	AS DbDataSource
        PROTECTED _nameOfTable		AS STRING
        PROTECTED _bindingSource	AS System.Windows.Forms.BindingSource

            // This is the source of data to which the Grid control is bound
        PROPERTY RecordSource AS STRING
            SET
                IF !String.IsNullOrEmpty( VALUE )
                    IF SELF:DesignMode
                        _nameOfTable := VALUE
                    ELSE
                        TRY
                            VAR current := DbGetSelect()
                            IF DbSelectArea( VALUE )
                                SELF:_nameOfTable := VALUE
                                SELF:_currentSource := DbDataSource()
                                IF SELF:_currentSource != NULL
                                    SELF:_currentSource:ShowDeleted := FALSE
                                    SELF:_currentSource:ShowRecno := FALSE
                                    DbSelectArea( current )
                                    // Now attach the DataTable to the DataGridView as DataSource
                                    SELF:_bindingSource := System.Windows.Forms.BindingSource{}
                                    SELF:_bindingSource:DataSource := SELF:_currentSource
                                    SELF:DataSource := SELF:_bindingSource
                                    //
                                    SELF:CreateDataColumns()
                                ENDIF
                            ENDIF
                        CATCH
                            NOP
                        END TRY
                    ENDIF
                    //
                ENDIF
            END SET

            GET
                IF SELF:DesignMode
                    RETURN _nameOfTable
                ELSE
                    IF _currentSource != NULL
                        RETURN _nameOfTable
                    ENDIF
                    RETURN String.Empty
                ENDIF
            END GET

        END PROPERTY

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
            IF SELF:DataSource != NULL
                IF SELF:DataSource IS BindingSource VAR BngSrc
                    IF BngSrc:DataSource IS DbDataSource VAR DbfDataSource
                        //
                        VAR _oRDD := (IRdd)DbfDataSource:SyncRoot
                        //
                        LOCAL f AS INT
                        LOCAL fieldCount := _oRDD:FieldCount AS LONG
                        // If ColumnCount < FieldCount
                        IF fieldCount > SELF:ColumnCount
                            SELF:_SetColumnCount( fieldCount )
                        ENDIF
                        FOR f:=1 UPTO fieldCount
                            LOCAL oCol    AS Column
                            // Get the existing Column, and Set it's value
                            oCol := (Column)SELF:Columns[ f-1 ]
                            oCol:DataPropertyName := _oRDD:FieldName(f)
                            oCol:HeaderText := _oRDD:FieldName(f)
                            oCol:Name := _oRDD:FieldName(f)
                        NEXT
                    ENDIF
                ENDIF
            ENDIF


        PRIVATE _VFPAfterRowColChange AS VFPOverride
        PROPERTY vfpAfterRowColChange AS STRING GET _VFPAfterRowColChange?:SendTo SET Set_AfterRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_AfterRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPAfterRowColChange := methodCall

        PRIVATE _VFPBeforeRowColChange AS VFPOverride
        PROPERTY vfpBeforeRowColChange AS STRING GET _VFPBeforeRowColChange?:SendTo SET Set_BeforeRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_BeforeRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPBeforeRowColChange := methodCall

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
            IF SELF:_VFPBeforeRowColChange != NULL
                SELF:_VFPBeforeRowColChange:Call( <USUAL>{nRowColIndex} )
            ENDIF
            IF SELF:_VFPAfterRowColChange != NULL
                SELF:_VFPAfterRowColChange:Call( <USUAL>{nRowColIndex} )
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

        METHOD Refresh() AS VOID
            LOCAL ds AS BindingSource
            //
            TRY
                ds := SELF:DataSource
                IF SELF:_currentSource != NULL
                    ds:Position := SELF:_currentSource:RecNo
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
                    nRow := SELF:CurrentCell:RowIndex
                    // One-based
                    nRow += 1
                ENDIF
                RETURN nRow
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
        PROPERTY ReadOnly AS LOGIC AUTO
        PROPERTY HighlightStyle AS LONG AUTO
        PROPERTY AllowAutoColumnFit AS LONG AUTO
        PROPERTY Themes AS LOGIC AUTO

    END CLASS
END NAMESPACE // XSharp.VFP.UI