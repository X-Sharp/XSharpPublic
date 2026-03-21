// ComboBox.prg
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
    /// The VFP compatible ComboBox class.
    /// </summary>
	PARTIAL CLASS ComboBox INHERIT System.Windows.Forms.ComboBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _uValue AS USUAL
		PRIVATE _rowSource AS STRING
		PRIVATE _rowSourceType AS INT
		PRIVATE _columnCount AS INT
		PRIVATE _boundColumn AS INT
		PRIVATE _vfpStyle AS INT
		PRIVATE _listIndex AS INT
		PRIVATE _displayCount AS INT

		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100,24}
			SELF:_uValue := NIL
			SELF:_rowSource := ""
			SELF:_rowSourceType := 0
			SELF:_columnCount := 1
			SELF:_boundColumn := 1
			SELF:_vfpStyle := 0
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
		/// Gets or sets the style of the combo box.
		/// 0=Dropdown, 1=DropdownList.
		/// Equivalent to VFP's Style property.
		/// </summary>
		/// <value>The combo box style (0 or 1). Default is 0.</value>
		[Category("VFP Properties"), Description("Combo box style: 0=Dropdown, 1=DropdownList")];
		[DefaultValue(0)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET
			SET
				SELF:_vfpStyle := VALUE
				SWITCH VALUE
					CASE 0
						SELF:DropDownStyle := ComboBoxStyle.DropDown
					CASE 1
						SELF:DropDownStyle := ComboBoxStyle.DropDownList
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

		PROPERTY DisplayCount AS LONG AUTO
		PROPERTY InputMask AS STRING AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY OLEDropTextInsertion AS LONG AUTO

		PROPERTY Picture AS STRING AUTO
		PROPERTY PictureSelectionDisplay  AS LONG AUTO
		PROPERTY ReadOnly AS LOGIC AUTO

		PROPERTY SelLength AS LONG GET SELF:SelectionLength SET SELF:SelectionLength := Value
		PROPERTY SelStart AS LONG GET SELF:SelectionStart SET SELF:SelectionStart := Value
		PROPERTY SelText AS STRING GET SELF:SelectedText  SET SelectedText  := Value
		PROPERTY SelectOnEntry AS LOGIC AUTO

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
		/// VFP uses 1-based indexing; this method converts to 0-based for .NET ComboBox.
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
		/// VFP uses 1-based indexing; this method converts to 0-based for .NET ComboBox.
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
		/// Clears the Items collection, resets ListIndex to -1, and clears the text field.
		/// </remarks>
		PUBLIC METHOD Clear() AS VOID STRICT
			SELF:Items:Clear()
			SELF:_listIndex := -1
			SELF:_uValue := NIL
			SELF:Text := ""
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
