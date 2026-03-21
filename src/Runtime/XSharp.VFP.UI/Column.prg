// Column.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.



USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Column class.
	/// Represents a single column in a Grid control with support for sorting, filtering, and visual customization.
	/// </summary>
	/// <remarks>
	/// A Column is a container within a Grid that displays data in a vertical arrangement.
	/// Each Column can contain controls (TextBox, ComboBox, etc.) and has a Header object.
	/// Columns support sorting, filtering, reordering, and resizing.
	/// VFP Help Reference: https://github.com/VFPX/HelpFile/tree/master/sources/dv_foxhelp
	/// </remarks>
	PARTIAL CLASS Column INHERIT System.Windows.Forms.DataGridViewTextBoxColumn IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		// ============================================================================
		// PHASE C: Column Enhancements
		// Reference: VFP 9 SP2 Help File - Grid/Column integration
		// ============================================================================

		// Sorting support backing fields
		PRIVATE _allowSorting := TRUE AS LOGIC
		PRIVATE _sortOrder := 0 AS INT  // 0=None, 1=Ascending, 2=Descending

		// Column management backing fields
		PRIVATE _columnReorder := TRUE AS LOGIC
		PRIVATE _columnResize := TRUE AS LOGIC
		PRIVATE _autoColumnWidth := FALSE AS LOGIC
		PRIVATE _minWidth := 20 AS INT
		PRIVATE _maxWidth := 1000 AS INT

		// Filtering support backing fields
		PRIVATE _allowFiltering := FALSE AS LOGIC
		PRIVATE _filterType := 0 AS INT  // 0=Text, 1=Numeric, 2=Date, 3=Logical

		// Visual properties backing fields
		PRIVATE _sortIndicator := TRUE AS LOGIC
		PRIVATE _headerColor := System.Drawing.SystemColors.ButtonFace AS System.Drawing.Color
		PRIVATE _headerAlignment := System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter AS System.Windows.Forms.DataGridViewContentAlignment

		CONSTRUCTOR(  )
            SUPER()
            SELF:Width := 75
            SELF:DefaultCellStyle:Alignment := System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
			RETURN

			// Todo
		PROPERTY ControlSource AS STRING AUTO

		// ============================================================================
		// Sorting Support Properties
		// ============================================================================

		/// <summary>
		/// Allow this column to be sorted when clicking the column header
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Allow column to be sorted")];
		[System.ComponentModel.DefaultValue(TRUE)];
		PROPERTY AllowSorting AS LOGIC
			GET
				RETURN _allowSorting
			END GET
			SET
				_allowSorting := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Current sort order for this column
		/// 0 = No sort, 1 = Ascending, 2 = Descending
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Sort order (0=None, 1=Ascending, 2=Descending)")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY SortOrder AS INT
			GET
				RETURN _sortOrder
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE <= 2
					_sortOrder := VALUE
				ENDIF
			END SET
		END PROPERTY

		// ============================================================================
		// Column Management Properties
		// ============================================================================

		/// <summary>
		/// Allow column to be reordered by dragging the header
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Allow column to be reordered via drag")];
		[System.ComponentModel.DefaultValue(TRUE)];
		PROPERTY ColumnReorder AS LOGIC
			GET
				RETURN _columnReorder
			END GET
			SET
				_columnReorder := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Allow column width to be resized by dragging the edge
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Allow column width to be resized")];
		[System.ComponentModel.DefaultValue(TRUE)];
		PROPERTY ColumnResize AS LOGIC
			GET
				RETURN _columnResize
			END GET
			SET
				_columnResize := VALUE
				SELF:Resizable := IF(VALUE, System.Windows.Forms.DataGridViewTriState.True, System.Windows.Forms.DataGridViewTriState.False)
			END SET
		END PROPERTY

		/// <summary>
		/// Automatically fit column width to content
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Auto-fit column width to content")];
		[System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY AutoColumnWidth AS LOGIC
			GET
				RETURN _autoColumnWidth
			END GET
			SET
				_autoColumnWidth := VALUE
				IF VALUE
					SELF:AutoSizeMode := System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
				ELSE
					SELF:AutoSizeMode := System.Windows.Forms.DataGridViewAutoSizeColumnMode.None
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Minimum column width in pixels
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Minimum column width in pixels")];
		[System.ComponentModel.DefaultValue(20)];
		PROPERTY MinWidth AS INT
			GET
				RETURN _minWidth
			END GET
			SET
				IF VALUE >= 10 .AND. VALUE <= 500
					_minWidth := VALUE
					IF SELF:Width < VALUE
						SELF:Width := VALUE
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Maximum column width in pixels
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Maximum column width in pixels")];
		[System.ComponentModel.DefaultValue(1000)];
		PROPERTY MaxWidth AS INT
			GET
				RETURN _maxWidth
			END GET
			SET
				IF VALUE >= 100 .AND. VALUE <= 5000
					_maxWidth := VALUE
					IF SELF:Width > VALUE
						SELF:Width := VALUE
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		// ============================================================================
		// Filtering Support Properties
		// ============================================================================

		/// <summary>
		/// Allow column to be filtered
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Allow column to be filtered")];
		[System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY AllowFiltering AS LOGIC
			GET
				RETURN _allowFiltering
			END GET
			SET
				_allowFiltering := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Filter type for this column (0=Text, 1=Numeric, 2=Date, 3=Logical)
		/// Determines what kind of filtering UI to show
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Filter type (0=Text, 1=Numeric, 2=Date, 3=Logical)")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY FilterType AS INT
			GET
				RETURN _filterType
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE <= 3
					_filterType := VALUE
				ENDIF
			END SET
		END PROPERTY

		// ============================================================================
		// Visual Properties
		// ============================================================================

		/// <summary>
		/// Show sort direction indicator (up/down arrow) in header
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Show sort direction indicator in header")];
		[System.ComponentModel.DefaultValue(TRUE)];
		PROPERTY SortIndicator AS LOGIC
			GET
				RETURN _sortIndicator
			END GET
			SET
				_sortIndicator := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Header background color for this column
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Header background color")];
		PROPERTY HeaderColor AS System.Drawing.Color
			GET
				RETURN _headerColor
			END GET
			SET
				_headerColor := VALUE
				IF SELF:HeaderCell != NULL
					SELF:HeaderCell:Style:BackColor := VALUE
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Header text alignment
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Header text alignment")];
		PROPERTY HeaderAlignment AS System.Windows.Forms.DataGridViewContentAlignment
			GET
				RETURN _headerAlignment
			END GET
			SET
				_headerAlignment := VALUE
				IF SELF:HeaderCell != NULL
					SELF:HeaderCell:Style:Alignment := VALUE
				ENDIF
			END SET
		END PROPERTY

		PROPERTY Font AS System.Drawing.Font GET SELF:DefaultCellStyle:Font SET SELF:DefaultCellStyle:Font := VALUE

		PROPERTY Header AS Header
		GET
			IF SELF:HeaderCell IS Header
				RETURN (Header)SELF:HeaderCell
			ENDIF
			VAR oHeader := Header{ SELF:HeaderCell }
			SELF:HeaderCell := oHeader
			RETURN oHeader
		END GET
		SET
			SELF:HeaderCell := VALUE
		END SET
		END PROPERTY

		PROPERTY TextBox AS TextBox AUTO

#include "FontProperties.xh"



	END CLASS
END NAMESPACE // XSharp.VFP.UI
