// Page.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// VFP Page Class Implementation
// Reference: VFP 9 SP2 Help File
// https://github.com/VFPX/HelpFile/tree/master/sources/dv_foxhelp
// Page Object GUID: 83a4017f-483a-4c8f-b63c-e16a58e011af

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Page class.
	/// A Page is a container object within a PageFrame that contains controls.
	/// </summary>
	/// <remarks>
	/// Pages are used to create tabbed forms or dialog boxes. You can refer to a page
	/// by name (PageFrame.PageName) or by index (PageFrame.Pages(n)).
	/// Only the active page is refreshed when the Form.Refresh method occurs.
	/// VFP Help Reference: https://github.com/VFPX/HelpFile/blob/master/sources/dv_foxhelp/html/83a4017f-483a-4c8f-b63c-e16a58e011af.htm
	/// </remarks>
	PARTIAL CLASS Page INHERIT System.Windows.Forms.Panel

		// Common properties that all VFP Objects support
		// #include "Headers\VFPObject.xh" // Already included in VFPContainer.xh that is in Generated/Page.generated.prg

// #include "XSharp\VFPProperties.xh"  // Already included in VFPContainer.xh that is in Generated/Page.generated.prg

		#include "Headers\ControlProperties.xh"
        // #include ".\Headers\ControlFocus.xh"
		#include "Headers\Tooltips.xh"

		// Page-specific property: Order of the page in the PageFrame
		// This is different from the index - PageOrder determines visual order
		// Reference: VFP Help - PageOrder property determines display order
		PRIVATE _pageOrder := 0 AS INT
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Determines the display order of the page within the PageFrame")];
		[System.ComponentModel.DefaultValue(0)];
		PROPERTY PageOrder AS INT
			GET
				RETURN _pageOrder
			END GET
			SET
				_pageOrder := VALUE
				// Update parent PageFrame if available
				IF SELF:Parent IS PageFrame
					VAR pageFrame := (PageFrame)SELF:Parent
					pageFrame:RefreshPageOrder()
				ENDIF
			END SET
		END PROPERTY

		// Picture property for page background
		// Not commonly used but part of VFP specification
		PRIVATE _picture AS STRING
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Specifies a background picture for the page")];
		[System.ComponentModel.DefaultValue(NULL)];
		PROPERTY Picture AS STRING
			GET
				RETURN _picture
			END GET
			SET
				_picture := VALUE
				// TODO: Implement picture loading if needed
			END SET
		END PROPERTY

		/// <summary>
		/// Constructor for Page class
		/// </summary>
		CONSTRUCTOR() STRICT
			SUPER()
			// Initialize as a transparent panel
			SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			SELF:AutoScroll := TRUE

			// Set default size
			SELF:Size := Size{200, 200}

			// Initialize properties
			_pageOrder := 0
			_picture := NULL

		RETURN

		/// <summary>
		/// Override OnPaint to handle transparency and custom drawing
		/// </summary>
		OVERRIDE PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
			// Call base implementation
			SUPER:OnPaint(e)

			// TODO: Implement picture drawing if Picture property is set
			IF !String.IsNullOrEmpty(_picture)
				// Picture drawing would go here
				// This is optional as pages typically don't show backgrounds in VFP forms
                NOP
			ENDIF

		RETURN

		/// <summary>
		/// Sets focus to this page (makes it active in its PageFrame)
		/// Reference: VFP Help - SetFocus method
		/// </summary>
		METHOD SetFocus() AS VOID STRICT
			// If this page is part of a PageFrame, activate it
			IF SELF:Parent IS PageFrame
				VAR pageFrame := (PageFrame)SELF:Parent
				pageFrame:ActivePage := pageFrame:GetPageIndex(SELF)
			ENDIF

			// Set focus to this control
			SELF:Focus()

		RETURN

	END CLASS

END NAMESPACE

