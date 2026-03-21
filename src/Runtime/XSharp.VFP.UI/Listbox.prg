// Listbox.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Listbox class.
	/// </summary>
	PARTIAL CLASS ListBox INHERIT System.Windows.Forms.ListBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _uValue AS USUAL
		PRIVATE _rowSource AS STRING
		PRIVATE _rowSourceType AS INT
		PRIVATE _columnCount AS INT
		PRIVATE _columnWidths AS STRING
		PRIVATE _boundColumn AS INT
		PRIVATE _multiSelect AS INT
		PRIVATE _listIndex AS INT
		PRIVATE _displayCount AS INT

		/// <summary>
		/// Constructor for ListBox control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100,170}
			SELF:_uValue := NIL
			SELF:_rowSource := ""
			SELF:_rowSourceType := 0
			SELF:_columnCount := 1
			SELF:_columnWidths := ""
			SELF:_boundColumn := 1
			SELF:_multiSelect := 0
			SELF:_listIndex := -1
			SELF:_displayCount := 0
			RETURN

		#include ".\Headers\ControlProperties.xh"
        #include ".\Headers\ControlFocus.xh"
		#include ".\Headers\ControlSource.xh"

		/// <summary>
		/// Gets or sets the value of the selected item.
		/// In VFP, this returns the value of the BoundColumn.
		/// </summary>
		/// <value>The selected item value as USUAL.</value>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY Value AS USUAL
			GET
				RETURN SELF:_uValue
			END GET
			SET
				IF !IsNil(VALUE)
					SELF:_uValue := VALUE
					// Try to find and select matching item
					SELF:SelectByValue(VALUE)
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the row source for the list.
		/// Equivalent to VFP's RowSource property.
		/// </summary>
		/// <value>The row source string.</value>
		[Category("VFP Properties"), Description("Data source for list items")];
		[DefaultValue("")];
		PROPERTY RowSource AS STRING
			GET
				RETURN SELF:_rowSource
			END GET
			SET
				SELF:_rowSource := VALUE
				SELF:PopulateFromRowSource()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the type of row source.
		/// 0=None, 1=Value, 5=Array.
		/// Equivalent to VFP's RowSourceType property.
		/// </summary>
		/// <value>The row source type (0, 1, or 5). Default is 0.</value>
		[Category("VFP Properties"), Description("Row source type: 0=None, 1=Value, 5=Array")];
		[DefaultValue(0)];
		PROPERTY RowSourceType AS INT
			GET
				RETURN SELF:_rowSourceType
			END GET
			SET
				SELF:_rowSourceType := VALUE
				SELF:PopulateFromRowSource()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the number of columns in the list.
		/// Equivalent to VFP's ColumnCount property.
		/// </summary>
		/// <value>The number of columns. Default is 1.</value>
		[Category("VFP Properties"), Description("Number of columns in the list")];
		[DefaultValue(1)];
		PROPERTY ColumnCount AS INT
			GET
				RETURN SELF:_columnCount
			END GET
			SET
				SELF:_columnCount := VALUE
				// Update column widths if needed
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the column widths as a comma-separated string.
		/// Equivalent to VFP's ColumnWidths property.
		/// </summary>
		/// <value>Comma-separated column widths in pixels.</value>
		[Category("VFP Properties"), Description("Column widths in pixels, comma-separated")];
		[DefaultValue("")];
		PROPERTY ColumnWidths AS STRING
			GET
				RETURN SELF:_columnWidths
			END GET
			SET
				SELF:_columnWidths := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets which column provides the value.
		/// Equivalent to VFP's BoundColumn property.
		/// </summary>
		/// <value>The column index (1-based). Default is 1.</value>
		[Category("VFP Properties"), Description("Which column returns as Value")];
		[DefaultValue(1)];
		PROPERTY BoundColumn AS INT
			GET
				RETURN SELF:_boundColumn
			END GET
			SET
				SELF:_boundColumn := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the multi-select mode.
		/// 0=None, 1=Simple, 2=Extended.
		/// Equivalent to VFP's MultiSelect property.
		/// </summary>
		/// <value>Multi-select mode (0, 1, or 2). Default is 0.</value>
		[Category("VFP Properties"), Description("Multi-select: 0=None, 1=Simple, 2=Extended")];
		[DefaultValue(0)];
		PROPERTY MultiSelect AS INT
			GET
				RETURN SELF:_multiSelect
			END GET
			SET
				SELF:_multiSelect := VALUE
				SWITCH VALUE
					CASE 0
						SELF:SelectionMode := SelectionMode.One
					CASE 1
						SELF:SelectionMode := SelectionMode.MultiSimple
					CASE 2
						SELF:SelectionMode := SelectionMode.MultiExtended
				END SWITCH
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the current selected index.
		/// Equivalent to VFP's ListIndex property.
		/// </summary>
		/// <value>The selected index (0-based). -1 if no selection.</value>
		[Category("VFP Properties"), Description("Current selected index")];
		[DefaultValue(-1)];
		PROPERTY ListIndex AS INT
			GET
				RETURN SELF:SelectedIndex
			END GET
			SET
				SELF:_listIndex := VALUE
				IF VALUE >= 0 .AND. VALUE < SELF:Items:Count
					SELF:SelectedIndex := VALUE
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets the number of items in the list.
		/// Equivalent to VFP's ListCount property.
		/// </summary>
		/// <value>The total number of items.</value>
		[Category("VFP Properties"), Description("Total number of items in the list")];
		[DefaultValue(0)];
		PROPERTY ListCount AS INT
			GET
				RETURN SELF:Items:Count
			END GET
		END PROPERTY

		/// <summary>
		/// Gets or sets the maximum number of items to display.
		/// Equivalent to VFP's DisplayCount property.
		/// </summary>
		/// <value>Maximum visible items. 0 for all items.</value>
		[Category("VFP Properties"), Description("Maximum number of visible items")];
		[DefaultValue(0)];
		PROPERTY DisplayCount AS INT
			GET
				RETURN SELF:_displayCount
			END GET
			SET
				SELF:_displayCount := VALUE
			END SET
		END PROPERTY

		PROPERTY AutoHideScrollBar AS LONG AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY Picture AS STRING AUTO

		/// <summary>
		/// Populates the list from the RowSource based on RowSourceType.
		/// </summary>
		PRIVATE METHOD PopulateFromRowSource() AS VOID
			SELF:Items:Clear()
			IF String.IsNullOrEmpty(SELF:_rowSource) .OR. SELF:_rowSourceType == 0
				RETURN
			ENDIF

		SWITCH SELF:_rowSourceType
			CASE 1  // Value - comma-separated
				VAR values := SELF:_rowSource:Split( c',' )
				FOREACH VAR val IN values
					SELF:Items:Add(val:Trim())
				NEXT
		END SWITCH
		END METHOD

		/// <summary>
		/// Selects an item by its value (searching in BoundColumn).
		/// </summary>
		PRIVATE METHOD SelectByValue(value AS USUAL) AS VOID
			VAR valueStr := Str( value )
			FOR VAR i := 0 TO SELF:Items:Count - 1
				IF SELF:Items[i]:ToString() == valueStr
					SELF:SelectedIndex := i
					SELF:_uValue := value
					RETURN
				ENDIF
			NEXT
		END METHOD

		/// <summary>
		/// Adds a new item to the list at the specified 1-based position.
		/// Equivalent to VFP's AddItem method.
		/// </summary>
		/// <param name="cItem">The item text to add.</param>
		/// <param name="nIndex">Optional 1-based position (default: append to end). Use -1 to append.</param>
		/// <remarks>
		/// VFP uses 1-based indexing; this method converts to 0-based for .NET ListBox.
		/// If nIndex is out of range, item is appended to the end.
		/// </remarks>
		PUBLIC METHOD AddItem(cItem AS STRING, nIndex AS INT := -1) AS VOID STRICT
			IF String.IsNullOrEmpty(cItem)
				cItem := ""
			ENDIF

			// Convert VFP 1-based index to 0-based .NET index
			IF nIndex <= 0 .OR. nIndex > SELF:Items:Count + 1
				// Append to end
				SELF:Items:Add(cItem)
			ELSE
				// Insert at specific position (convert 1-based to 0-based)
				SELF:Items:Insert(nIndex - 1, cItem)
			ENDIF
		END METHOD

		/// <summary>
		/// Removes an item from the list by its 1-based index.
		/// Equivalent to VFP's RemoveItem method.
		/// </summary>
		/// <param name="nIndex">The 1-based index of the item to remove.</param>
		/// <remarks>
		/// VFP uses 1-based indexing; this method converts to 0-based for .NET ListBox.
		/// If nIndex is out of range, nothing happens.
		/// </remarks>
		PUBLIC METHOD RemoveItem(nIndex AS INT) AS VOID STRICT
			// Convert VFP 1-based index to 0-based .NET index
			VAR zeroBasedIndex := nIndex - 1
			IF zeroBasedIndex >= 0 .AND. zeroBasedIndex < SELF:Items:Count
				SELF:Items:RemoveAt(zeroBasedIndex)
				// If removed item was selected, update ListIndex
				IF SELF:SelectedIndex >= SELF:Items:Count .AND. SELF:Items:Count > 0
					SELF:SelectedIndex := SELF:Items:Count - 1
				ENDIF
			ENDIF
		END METHOD

		/// <summary>
		/// Removes all items from the list.
		/// Equivalent to VFP's Clear method.
		/// </summary>
		/// <remarks>
		/// Clears the Items collection and resets ListIndex to -1.
		/// </remarks>
		PUBLIC METHOD Clear() AS VOID STRICT
			SELF:Items:Clear()
			SELF:_listIndex := -1
			SELF:_uValue := NIL
		END METHOD

		/// <summary>
		/// Finds an item in the list that matches the search string.
		/// Equivalent to VFP's FindString method.
		/// </summary>
		/// <param name="cSearchString">The text to search for (case-insensitive, partial match).</param>
		/// <param name="nStartIndex">Optional 0-based starting index. Default is 0 (start from beginning).</param>
		/// <returns>The 1-based index of the found item, or -1 if not found.</returns>
		/// <remarks>
		/// Searches for items that start with cSearchString (case-insensitive).
		/// Returns VFP 1-based index (add 1 to .NET 0-based index).
		/// </remarks>
		PUBLIC METHOD FindString(cSearchString AS STRING, nStartIndex AS INT := 0) AS INT STRICT
			IF String.IsNullOrEmpty(cSearchString)
				RETURN -1
			ENDIF

			VAR searchUpper := cSearchString:ToUpper()
			FOR VAR i := nStartIndex TO SELF:Items:Count - 1
				VAR itemText := SELF:Items[i]:ToString():ToUpper()
				IF itemText:StartsWith(searchUpper)
					// Return 1-based index
					RETURN i + 1
				ENDIF
			NEXT

			// Not found
			RETURN -1
		END METHOD

	END CLASS
END NAMESPACE // XSharp.VFP.UI
