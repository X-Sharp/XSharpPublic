// PageFrame.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// VFP PageFrame Class Implementation
// Reference: VFP 9 SP2 Help File
// https://github.com/VFPX/HelpFile/tree/master/sources/dv_foxhelp
// PageFrame Control GUID: 852d4c6e-2b5c-404b-ae44-90cdd79ebea5

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible PageFrame class.
	/// A PageFrame is a container object that contains Page objects which can contain controls.
	/// </summary>
	/// <remarks>
	/// The PageFrame defines global characteristics of the pages: size, positioning, border style,
	/// which page is active, and so on. The PageFrame determines the location of the pages and how
	/// much of each page is visible. Pages are positioned at the top left corner of the PageFrame.
	/// If the PageFrame is moved, the pages move with the PageFrame.
	///
	/// Only the active page is refreshed when the Form.Refresh method occurs.
	/// VFP Help Reference: https://github.com/VFPX/HelpFile/blob/master/sources/dv_foxhelp/html/852d4c6e-2b5c-404b-ae44-90cdd79ebea5.htm
	/// </remarks>
	PARTIAL CLASS PageFrame INHERIT System.Windows.Forms.TabControl

		// Common properties that all VFP Objects support
		// #include "Headers\VFPObject.xh" // Already included in VFPContainer.xh that is in Generated/PageFrame.generated.prg

// #include "XSharp\VFPProperties.xh"  // Already included in VFPContainer.xh that is in Generated/PageFrame.generated.prg


		#include "Headers\ControlProperties.xh"
        // #include ".\Headers\ControlFocus.xh"
		#include "Headers\Tooltips.xh"

		// ============================================================================
		// VFP PageFrame Properties
		// ============================================================================

		// The index (0-based) of the active/currently displayed page
		PRIVATE _activePage := 0 AS INT
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Gets or sets the index of the active page")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY ActivePage AS INT
			GET
				RETURN _activePage
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE < SELF:Controls:Count
					_activePage := VALUE
					SELF:SelectedIndex := VALUE
					// Trigger any necessary refresh for active page
					IF SELF:Controls:Count > VALUE
						VAR activePage := (Page)SELF:Controls[VALUE]
						activePage:Visible := TRUE
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		// BorderColor property for the PageFrame border
		PRIVATE _borderColor := 0 AS LONG
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Specifies the border color of the PageFrame")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY BorderColor AS LONG
			GET
				RETURN _borderColor
			END GET
			SET
				_borderColor := VALUE
			END SET
		END PROPERTY

		// BorderWidth property for the PageFrame border
		PRIVATE _borderWidth := 1 AS LONG
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Specifies the border width of the PageFrame")];
		[System.ComponentModel.DefaultValue(1)];
		PROPERTY BorderWidth AS LONG
			GET
				RETURN _borderWidth
			END GET
			SET
				_borderWidth := VALUE
			END SET
		END PROPERTY

		// Number of pages in the PageFrame (read-only)
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Gets the number of pages in the PageFrame")];
		PROPERTY PageCount AS LONG
			GET
				RETURN (LONG)SELF:Controls:Count
			END GET
		END PROPERTY

		// Height of pages within the PageFrame
		PRIVATE _pageHeight AS INT
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Height of the pages in the PageFrame")];
		PROPERTY PageHeight AS INT
			GET
				IF _pageHeight == 0
					// Return control height minus tab header height
					RETURN SELF:Height - 30
				ENDIF
				RETURN _pageHeight
			END GET
			SET
				_pageHeight := VALUE
				RefreshPageDimensions()
			END SET
		END PROPERTY

		// Width of pages within the PageFrame
		PRIVATE _pageWidth AS INT
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Width of the pages in the PageFrame")];
		PROPERTY PageWidth AS INT
			GET
				IF _pageWidth == 0
					RETURN SELF:Width
				ENDIF
				RETURN _pageWidth
			END GET
			SET
				_pageWidth := VALUE
				RefreshPageDimensions()
			END SET
		END PROPERTY

		// Collection of pages (compatible with VFP Pages collection)
		PRIVATE _pages AS List<Page>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Collection of Page objects in the PageFrame")];
		PROPERTY Pages AS USUAL
			GET
				// Return the pages collection
				// In VFP, you access pages by name or index
				RETURN SELF:_pages
			END GET
		END PROPERTY

		// Tab orientation: 0=Top, 1=Bottom, 2=Left, 3=Right
		PRIVATE _tabOrientation := 0 AS LONG
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Determines the position of the tabs (0=Top, 1=Bottom, 2=Left, 3=Right)")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY TabOrientation AS LONG
			GET
				RETURN _tabOrientation
			END GET
			SET
				_tabOrientation := VALUE
				// Update TabControl alignment based on value
				SWITCH VALUE
				CASE 0  // Top
					SELF:Alignment := TabAlignment.Top
				CASE 1  // Bottom
					SELF:Alignment := TabAlignment.Bottom
				CASE 2  // Left
					SELF:Alignment := TabAlignment.Left
				CASE 3  // Right
					SELF:Alignment := TabAlignment.Right
				END SWITCH
			END SET
		END PROPERTY

		// Whether tabs are visible
		PRIVATE _tabs := TRUE AS LOGIC
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Determines whether tabs are visible")];
		[System.ComponentModel.DefaultValue(TRUE)];
		PROPERTY Tabs AS LOGIC
			GET
				RETURN _tabs
			END GET
			SET
				_tabs := VALUE
				// Hide tabs by changing appearance if needed
				// TabControl in WinForms doesn't have a direct "hide tabs" option,
				// so we set ItemSize to 0 height or use appearance tricks
				IF !VALUE
					SELF:ItemSize := Size{0, 0}
				ELSE
					SELF:ItemSize := Size{0, 20}  // Default height
				ENDIF
			END SET
		END PROPERTY

		// Tab stretch: 0=None, 1=Justified
		PRIVATE _tabStretch := 0 AS LONG
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Determines tab stretching (0=None, 1=Justified)")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY TabStretch AS LONG
			GET
				RETURN _tabStretch
			END GET
			SET
				_tabStretch := VALUE
				IF VALUE == 1
					SELF:SizeMode := TabSizeMode.FillToRight
				ELSE
					SELF:SizeMode := TabSizeMode.Normal
				ENDIF
			END SET
		END PROPERTY

		// Tab style: 0=Buttons, 1=Tabs
		PRIVATE _tabStyle := 1 AS LONG
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Determines the tab style (0=Buttons, 1=Tabs)")];
		[System.ComponentModel.DefaultValue(1)];
		PROPERTY TabStyle AS LONG
			GET
				RETURN _tabStyle
			END GET
			SET
				_tabStyle := VALUE
			END SET
		END PROPERTY

		// ============================================================================
		// Constructor
		// ============================================================================

		/// <summary>
		/// Constructor for PageFrame class
		/// </summary>
		CONSTRUCTOR() STRICT
			SUPER()

			// Initialize pages collection
			_pages := List<Page>{}

			// Initialize properties
			_activePage := 0
			_borderColor := 0
			_borderWidth := 1
			_pageHeight := 0
			_pageWidth := 0
			_tabOrientation := 0
			_tabs := TRUE
			_tabStretch := 0
			_tabStyle := 1

			// Configure TabControl appearance
			SELF:Dock := DockStyle.None
			SELF:Size := Size{300, 300}
			SELF:Alignment := TabAlignment.Top
			SELF:SizeMode := TabSizeMode.Normal
			SELF:Multiline := FALSE

			// Subscribe to selection change event
			SELF:SelectedIndexChanged += SelectedIndexChangedHandler

		RETURN

		// ============================================================================
		// Methods
		// ============================================================================

		/// <summary>
		/// Adds a new Page to the PageFrame
		/// Reference: VFP Help - Adding Pages to PageFrame
		/// </summary>
		METHOD AddPage(pageName AS STRING) AS Page
			LOCAL newPage AS Page
			LOCAL tabPage AS TabPage

			// Create the Page object
			newPage := Page{}

			// Create corresponding TabPage for display
			tabPage := TabPage{}
			tabPage:Text := pageName
			tabPage:Name := pageName

			// Add the page to our collection
			_pages:Add(newPage)
			SELF:TabPages:Add(tabPage)

			// Add the Page as a control to the TabPage
			tabPage:Controls:Add(newPage)
			newPage:Dock := DockStyle.Fill
			newPage:PageOrder := _pages:Count - 1

			RETURN newPage
		END METHOD

		/// <summary>
		/// Gets the index of a given page
		/// </summary>
		METHOD GetPageIndex(page AS Page) AS INT
			RETURN _pages:IndexOf(page)
		END METHOD

		/// <summary>
		/// Refreshes the order of pages based on PageOrder property
		/// </summary>
		INTERNAL METHOD RefreshPageOrder() AS VOID
			// TODO: Implement page reordering if PageOrder properties change
			// This would involve reordering the TabPages
		END METHOD

		/// <summary>
		/// Refreshes page dimensions based on PageHeight and PageWidth properties
		/// </summary>
		INTERNAL METHOD RefreshPageDimensions() AS VOID
			FOREACH VAR page IN _pages
				page:Width := SELF:PageWidth
				page:Height := SELF:PageHeight
			NEXT
		END METHOD

		/// <summary>
		/// Handles TabControl selection changed event
		/// </summary>
		PRIVATE METHOD SelectedIndexChangedHandler(sender AS OBJECT, e AS EventArgs) AS VOID
			_activePage := SELF:SelectedIndex
			// Ensure only active page is visible
			LOCAL i AS INT
			FOR i := 0 UPTO _pages:Count - 1
				_pages[i]:Visible := (i == _activePage)
			NEXT
		END METHOD

		/// <summary>
		/// Sets focus to this PageFrame
		/// Reference: VFP Help - SetFocus method
		/// </summary>
		METHOD SetFocus() AS VOID STRICT
			SELF:Focus()
			// Also focus the active page's first control if available
			IF _activePage < _pages:Count
				VAR activePage := _pages[_activePage]
				activePage:Focus()
			ENDIF
		END METHOD

		/// <summary>
		/// Refresh method - refreshes only the active page
		/// Reference: VFP Help - Only the active page is refreshed
		/// </summary>
		OVERRIDE METHOD Refresh() AS VOID
			SUPER:Refresh()
			// Refresh only the active page
			IF _activePage < _pages:Count
				_pages[_activePage]:Refresh()
			ENDIF
		END METHOD

	END CLASS

END NAMESPACE
