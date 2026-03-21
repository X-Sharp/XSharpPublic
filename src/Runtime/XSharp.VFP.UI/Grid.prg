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
    CLASS Grid INHERIT System.Windows.Forms.DataGridView IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner



        PROPERTY HighlightBackColor AS LONG AUTO
        PROPERTY HighlightForeColor AS LONG AUTO

        PRIVATE _oldRowIndex AS LONG
        PRIVATE _oldColIndex AS LONG
        PRIVATE _rowColChange AS LONG

        // GridLines property backing field
        // Values: 0=None, 1=Horizontal, 2=Vertical, 3=Both
        PRIVATE _gridLines := 0 AS INT

        #include "Generated\VFPContainer.xh"
        #include "XSharp\VFPProperties.xh"

        #include ".\Headers\ControlProperties.xh"
        #include ".\Headers\ControlFocus.xh"

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
            // Add handler for column header clicks to enable sorting
            SELF:ColumnHeaderMouseClick += System.Windows.Forms.DataGridViewCellMouseEventHandler{ SELF, @OnColumnHeaderClick() }
            SELF:Size := System.Drawing.Size{320, 200}

            // Initialize grid properties with defaults
            SELF:ApplyGridLineStyle()

            RETURN

        /// <summary>
        /// Handle column header clicks for sorting
        /// </summary>
        PRIVATE METHOD OnColumnHeaderClick(sender AS OBJECT, e AS System.Windows.Forms.DataGridViewCellMouseEventArgs) AS VOID
            // Click on a column header initiates sorting
            IF e:ColumnIndex >= 0
                // Toggle sort order if clicking same column, otherwise sort ascending
                IF _sortColumn == e:ColumnIndex
                    _sortAscending := !_sortAscending
                ELSE
                    _sortAscending := TRUE
                ENDIF
                SELF:Sort(e:ColumnIndex, _sortAscending)
            ENDIF
        END METHOD

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

        // ============================================================================
        // GridLines Property - Controls visibility of grid lines
        // Values: 0=None, 1=Horizontal, 2=Vertical, 3=Both
        // Reference: VFP Help - GridLines determines which grid lines are visible
        // ============================================================================
        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Controls the display of grid lines (0=None, 1=Horizontal, 2=Vertical, 3=Both)")];
        [System.ComponentModel.DefaultValue(0)];
        PROPERTY GridLines AS INT
            GET
                RETURN _gridLines
            END GET
            SET
                _gridLines := VALUE
                SELF:ApplyGridLineStyle()
            END SET
        END PROPERTY

        /// <summary>
        /// Internal method to apply grid line style based on GridLines property
        /// </summary>
        PRIVATE METHOD ApplyGridLineStyle() AS VOID
            SWITCH _gridLines
                CASE 0
                    // None - no grid lines
                    SELF:CellBorderStyle := DataGridViewCellBorderStyle.None
                    SELF:GridColor := System.Drawing.Color.LightGray
                CASE 1
                    // Horizontal only
                    SELF:CellBorderStyle := DataGridViewCellBorderStyle.SingleHorizontal
                    SELF:GridColor := System.Drawing.Color.LightGray
                CASE 2
                    // Vertical only
                    SELF:CellBorderStyle := DataGridViewCellBorderStyle.SingleVertical
                    SELF:GridColor := System.Drawing.Color.LightGray
                CASE 3
                    // Both horizontal and vertical
                    SELF:CellBorderStyle := DataGridViewCellBorderStyle.Single
                    SELF:GridColor := System.Drawing.Color.LightGray
                OTHERWISE
                    // Default to both if invalid value
                    _gridLines := 3
                    SELF:CellBorderStyle := DataGridViewCellBorderStyle.Single
            END SWITCH
        END METHOD

        // ============================================================================
        // Column Freezing Support
        // ============================================================================
        PRIVATE _frozenColumnCount := 0 AS INT
        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Number of leftmost columns to freeze")];
        [System.ComponentModel.DefaultValue(0)];
        PROPERTY FrozenColumnCount AS INT
            GET
                RETURN _frozenColumnCount
            END GET
            SET
                IF VALUE >= 0 .AND. VALUE <= SELF:ColumnCount
                    _frozenColumnCount := VALUE
                    SELF:ApplyColumnFreeze()
                ENDIF
            END SET
        END PROPERTY

        /// <summary>
        /// Freeze specified number of columns (locked from horizontal scrolling)
        /// </summary>
        PUBLIC METHOD FreezeColumns(columnCount AS INT) AS VOID
            SELF:FrozenColumnCount := columnCount
        END METHOD

        /// <summary>
        /// Internal method to apply column freezing
        /// </summary>
        PRIVATE METHOD ApplyColumnFreeze() AS VOID
            TRY
                LOCAL i AS INT
                FOR i := 0 UPTO SELF:ColumnCount - 1
                    IF i < _frozenColumnCount
                        SELF:Columns[i]:Frozen := TRUE
                    ELSE
                        SELF:Columns[i]:Frozen := FALSE
                    ENDIF
                NEXT
            CATCH
                // Silently handle any errors during freezing
                NOP
            END TRY
        END METHOD

        // ============================================================================
        // Sorting Support
        // ============================================================================
        PRIVATE _sortColumn := -1 AS INT
        PRIVATE _sortAscending := TRUE AS LOGIC

        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Index of column used for sorting")];
        [System.ComponentModel.DefaultValue(-1)];
        PROPERTY SortColumn AS INT
            GET
                RETURN _sortColumn
            END GET
            SET
                _sortColumn := VALUE
            END SET
        END PROPERTY

        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Sort order (TRUE=Ascending, FALSE=Descending)")];
        [System.ComponentModel.DefaultValue(TRUE)];
        PROPERTY SortAscending AS LOGIC
            GET
                RETURN _sortAscending
            END GET
            SET
                _sortAscending := VALUE
            END SET
        END PROPERTY

        /// <summary>
        /// Apply sorting to the grid based on specified column
        /// </summary>
        PUBLIC METHOD Sort(columnIndex AS INT, ascending AS LOGIC) AS VOID
            _sortColumn := columnIndex
            _sortAscending := ascending

            TRY
                IF SELF:_bindingSource != NULL
                    IF columnIndex >= 0 .AND. columnIndex < SELF:ColumnCount
                        VAR columnName := SELF:Columns[columnIndex]:DataPropertyName
                        IF !String.IsNullOrEmpty(columnName)
                            VAR sortOrder := IF(ascending, " ASC", " DESC")
                            SELF:_bindingSource:Sort := columnName + sortOrder
                        ENDIF
                    ENDIF
                ENDIF
            CATCH
                // Silently handle sort errors
                NOP
            END TRY
        END METHOD

        // ============================================================================
        // Filtering Support
        // ============================================================================
        PRIVATE _filterExpression := "" AS STRING

        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Filter expression for data rows")];
        [System.ComponentModel.DefaultValue("")];
        PROPERTY FilterExpression AS STRING
            GET
                RETURN _filterExpression
            END GET
            SET
                _filterExpression := VALUE
                SELF:ApplyFilter()
            END SET
        END PROPERTY

        /// <summary>
        /// Apply filter to the grid data
        /// </summary>
        PUBLIC METHOD ApplyFilter() AS VOID
            TRY
                IF SELF:_bindingSource != NULL
                    IF String.IsNullOrEmpty(_filterExpression)
                        SELF:_bindingSource:Filter := NULL
                    ELSE
                        SELF:_bindingSource:Filter := _filterExpression
                    ENDIF
                ENDIF
            CATCH
                // Reset filter on error
                TRY
                    IF SELF:_bindingSource != NULL
                        SELF:_bindingSource:Filter := NULL
                    ENDIF
                CATCH
                    NOP
                END TRY
            END TRY
        END METHOD

        // ============================================================================
        // Additional Visual Properties
        // ============================================================================
        PRIVATE _headerHeight := 21 AS INT
        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Height of column headers in pixels")];
        [System.ComponentModel.DefaultValue(21)];
        PROPERTY HeaderHeight AS INT
            GET
                RETURN _headerHeight
            END GET
            SET
                _headerHeight := VALUE
                SELF:ColumnHeadersHeight := VALUE
            END SET
        END PROPERTY

        PRIVATE _highlightRow := FALSE AS LOGIC
        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("When TRUE, highlights entire row when selected")];
        [System.ComponentModel.DefaultValue(FALSE)];
        PROPERTY HighlightRow AS LOGIC
            GET
                RETURN _highlightRow
            END GET
            SET
                _highlightRow := VALUE
                IF VALUE
                    SELF:SelectionMode := DataGridViewSelectionMode.FullRowSelect
                ELSE
                    SELF:SelectionMode := DataGridViewSelectionMode.CellSelect
                ENDIF
            END SET
        END PROPERTY

        PRIVATE _alternatingRowColor := System.Drawing.Color.White AS System.Drawing.Color
        [System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Background color for alternating rows")];
        PROPERTY AlternatingRowColor AS System.Drawing.Color
            GET
                RETURN _alternatingRowColor
            END GET
            SET
                _alternatingRowColor := VALUE
                SELF:AlternatingRowsDefaultCellStyle:BackColor := VALUE
            END SET
        END PROPERTY

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
        PROPERTY vfpAfterRowColChange AS STRING GET _VFPAfterRowColChange?:SendTo SET SELF:Set_AfterRowColChange( VFPOverride{SELF, VALUE} )

        METHOD Set_AfterRowColChange( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPAfterRowColChange := methodCall

        PRIVATE _VFPBeforeRowColChange AS VFPOverride
        PROPERTY vfpBeforeRowColChange AS STRING GET _VFPBeforeRowColChange?:SendTo SET SELF:Set_BeforeRowColChange( VFPOverride{SELF, VALUE} )

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

        OVERRIDE METHOD Refresh() AS VOID
            LOCAL ds AS BindingSource
            //
            TRY
                ds := SELF:DataSource
                IF SELF:_currentSource != NULL
                    ds:Position := (INT) SELF:_currentSource:RecNo
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
        //OVERRIDE PROPERTY ReadOnly AS LOGIC AUTO
        PROPERTY HighlightStyle AS LONG AUTO
        PROPERTY AllowAutoColumnFit AS LONG AUTO
        PROPERTY Themes AS LOGIC AUTO

        /// <summary>
        /// Adds a new column to the Grid at the specified 1-based position.
        /// Equivalent to VFP's AddColumn method.
        /// </summary>
        /// <param name="cColumnName">The name of the new column.</param>
        /// <param name="nPosition">Optional 1-based position for the column. Default is append to end.</param>
        /// <returns>The newly created Column object.</returns>
        /// <remarks>
        /// Creates a new TextBoxColumn and inserts it at the specified position.
        /// If nPosition is out of range, appends to the end.
        /// VFP uses 1-based indexing; converted to 0-based for .NET DataGridView.
        /// </remarks>
        PUBLIC METHOD AddColumn(cColumnName AS STRING, nPosition AS INT := -1) AS Column STRICT
            VAR newColumn := Column{}
            newColumn:Name := cColumnName
            newColumn:HeaderText := cColumnName
            newColumn:CellTemplate := System.Windows.Forms.DataGridViewTextBoxCell{}
            newColumn:AutoSizeMode := DataGridViewAutoSizeColumnMode.None
            newColumn:Width := 100

            // Convert VFP 1-based index to 0-based .NET index
            IF nPosition <= 0 .OR. nPosition > SELF:Columns:Count + 1
                // Append to end
                SELF:Columns:Add(newColumn)
            ELSE
                // Insert at specific position (convert 1-based to 0-based)
                SELF:Columns:Insert(nPosition - 1, newColumn)
            ENDIF

            RETURN newColumn
        END METHOD

        /// <summary>
        /// Removes a column from the Grid by its 1-based index.
        /// Equivalent to VFP's RemoveColumn method.
        /// </summary>
        /// <param name="nPosition">The 1-based position of the column to remove.</param>
        /// <remarks>
        /// Removes the column at the specified position.
        /// VFP uses 1-based indexing; converted to 0-based for .NET DataGridView.
        /// If nPosition is out of range, nothing happens.
         /// </remarks>
         PUBLIC METHOD RemoveColumn(nPosition AS INT) AS VOID STRICT
             // Convert VFP 1-based index to 0-based .NET index
             VAR zeroBasedIndex := nPosition - 1
             IF zeroBasedIndex >= 0 .AND. zeroBasedIndex < SELF:Columns:Count
                 SELF:Columns:RemoveAt(zeroBasedIndex)
             ENDIF
         END METHOD

         /// <summary>
         /// Appends a new row to the Grid.
         /// Equivalent to VFP's AppendRow method.
         /// </summary>
         /// <returns>The index of the newly added row (0-based).</returns>
         /// <remarks>
         /// Adds a new empty row to the end of the Grid.
         /// </remarks>
         PUBLIC METHOD AppendRow() AS INT STRICT
             VAR newRowIndex := SELF:Rows:Add()
             RETURN newRowIndex
         END METHOD

         /// <summary>
         /// Deletes a row from the Grid by its 1-based index.
         /// Equivalent to VFP's DeleteRow method.
         /// </summary>
         /// <param name="nRowIndex">The 1-based index of the row to delete.</param>
         /// <remarks>
         /// Removes the row at the specified position.
         /// VFP uses 1-based indexing; converted to 0-based for .NET DataGridView.
         /// If nRowIndex is out of range, nothing happens.
         /// </remarks>
         PUBLIC METHOD DeleteRow(nRowIndex AS INT) AS VOID STRICT
             // Convert VFP 1-based index to 0-based .NET index
             VAR zeroBasedIndex := nRowIndex - 1
             IF zeroBasedIndex >= 0 .AND. zeroBasedIndex < SELF:Rows:Count
                 SELF:Rows:RemoveAt(zeroBasedIndex)
             ENDIF
         END METHOD

     END CLASS
END NAMESPACE // XSharp.VFP.UI
