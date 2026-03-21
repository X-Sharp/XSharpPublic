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

	END CLASS
END NAMESPACE // XSharp.VFP.UI
