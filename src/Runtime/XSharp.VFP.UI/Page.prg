USING System
USING System.ComponentModel
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// Page control - Embedded tab page within a PageFrame
/// Inherits from TabPage to provide VFP-style page properties
/// Used as a container for controls within a multi-tab interface
/// See Also: PageFrame for the parent TabControl
/// </summary>
PUBLIC CLASS Page INHERIT System.Windows.Forms.TabPage IMPLEMENTS IVFPObject, IVFPOwner

	#include "Headers/VFPContainer.xh"
	
	// ============================================================================
	// Include VFPObject base implementation (IVFPObject, IVFPHelp)
	// ============================================================================
	#include "Headers/VFPObject.xh"

	PRIVATE _pageIndex AS INT32
	PRIVATE _caption AS STRING
	PRIVATE _enabled AS LOGIC
	PRIVATE _baseClass AS STRING
	PRIVATE _class AS STRING
	PRIVATE _classLibrary AS STRING
	PRIVATE _comment AS STRING
	PRIVATE _helpContextID AS LONG
	PRIVATE _whatsThisHelpID AS LONG

	/// <summary>
	/// Gets or sets the caption of the page
	/// </summary>
	PUBLIC PROPERTY Caption AS STRING
		GET
			RETURN SELF:_caption
		END GET
		SET
			SELF:_caption := VALUE
			SELF:Text := VALUE
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets the page index (0-based)
	/// </summary>
	PUBLIC PROPERTY PageIndex AS INT32
		GET
			RETURN SELF:_pageIndex
		END GET
		SET
			SELF:_pageIndex := VALUE
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets whether the page is enabled
	/// </summary>
	PUBLIC PROPERTY PageEnabled AS LOGIC
		GET
			RETURN SELF:_enabled
		END GET
		SET
			SELF:_enabled := VALUE
			SELF:Enabled := VALUE
		END SET
	END PROPERTY

	/// <summary>
	/// Gets the controls collection for this page
	/// </summary>
	PUBLIC PROPERTY PageControls AS Control.ControlCollection
		GET
			RETURN SELF:Controls
		END GET
	END PROPERTY

	/// <summary>
	/// Constructor - initializes the Page with defaults
	/// </summary>
	PUBLIC CONSTRUCTOR()
		SUPER()
		SELF:_pageIndex := 0
		SELF:_caption := "Page"
		SELF:_enabled := TRUE
		SELF:_baseClass := "Page"
		SELF:_class := "Page"
		SELF:_classLibrary := ""
		SELF:_comment := ""
		SELF:_helpContextID := 0
		SELF:_whatsThisHelpID := 0
		SELF:Text := SELF:_caption
		SELF:BackColor := System.Drawing.SystemColors.Control
		SELF:Enabled := TRUE
		SELF:AutoScroll := TRUE
	END CONSTRUCTOR

	/// <summary>
	/// Constructor with caption parameter
	/// </summary>
	PUBLIC CONSTRUCTOR(cCaption AS STRING)
		SELF:Constructor()
		SELF:Caption := cCaption
	END CONSTRUCTOR



	/// <summary>
	/// Constructor with caption and index parameters
	/// </summary>
	PUBLIC CONSTRUCTOR(cCaption AS STRING, nIndex AS INT32)
		SELF:Constructor(cCaption)
		SELF:PageIndex := nIndex
	END CONSTRUCTOR

	/// <summary>
	/// Initializes the page with complete properties
	/// </summary>
	PUBLIC METHOD Initialize(cCaption AS STRING, nIndex AS INT32, lEnabled AS LOGIC) AS VOID
		SELF:Caption := cCaption
		SELF:PageIndex := nIndex
		SELF:PageEnabled := lEnabled
	END METHOD

	/// <summary>
	/// Activates this page (makes it the active tab)
	/// </summary>
	PUBLIC METHOD Activate() AS VOID
		IF SELF:Parent != NULL .AND. SELF:Parent IS TabControl
			VAR tc := (TabControl)SELF:Parent
			IF tc:TabPages:Contains(SELF)
				tc:SelectedTab := SELF
			ENDIF
		ENDIF
	END METHOD

	/// <summary>
	/// Clears all controls from this page
	/// </summary>
	PUBLIC METHOD ClearControls() AS VOID
		LOCAL i AS INT32
		FOR i := SELF:Controls:Count - 1 DOWNTO 0
			SELF:Controls[i]:Dispose()
		NEXT
		SELF:Controls:Clear()
	END METHOD

	/// <summary>
	/// Gets a control from the page by name
	/// </summary>
	PUBLIC METHOD GetControl(cControlName AS STRING) AS Control
		FOREACH ctrl AS Control IN SELF:Controls
			IF ctrl:Name == cControlName
				RETURN ctrl
			ENDIF
		NEXT
		RETURN NULL
	END METHOD

	/// <summary>
	/// Resets page to default appearance
	/// </summary>
	PUBLIC METHOD Reset() AS VOID
		SELF:ClearControls()
		SELF:Caption := "Page"
		SELF:PageIndex := 0
		SELF:PageEnabled := TRUE
		SELF:BackColor := System.Drawing.SystemColors.Control
	END METHOD

END CLASS

END NAMESPACE
