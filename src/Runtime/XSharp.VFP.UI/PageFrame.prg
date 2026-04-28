USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Linq
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// PageFrame control - Multi-tab container for Page objects
/// Inherits from TabControl to provide VFP-style tab management
/// Supports dynamic page creation, removal, and page object management
/// See Also: Page for individual tab page control
/// </summary>
PUBLIC CLASS PageFrame INHERIT System.Windows.Forms.TabControl IMPLEMENTS IVFPObject, IVFPOwner

	#include "Headers/VFPContainer.xh"

	// ============================================================================
	// Include VFPObject base implementation (IVFPObject, IVFPHelp)
	// ============================================================================
	#include "Headers/VFPObject.xh"

	PRIVATE _pages AS List<Page>
	PRIVATE _pageCount AS INT32
	PRIVATE _activePage AS INT32
	PRIVATE _objectsCache AS Dictionary<STRING, OBJECT>
	PRIVATE _baseClass AS STRING
	PRIVATE _class AS STRING
	PRIVATE _classLibrary AS STRING
	PRIVATE _comment AS STRING
	PRIVATE _helpContextID AS LONG
	PRIVATE _whatsThisHelpID AS LONG

	/// <summary>
	/// Gets the number of pages in the frame
	/// </summary>
	PUBLIC PROPERTY PageCount AS INT32
		GET
			RETURN SELF:TabPages:Count
		END GET
	END PROPERTY

	/// <summary>
	/// Gets or sets the active page index
	/// </summary>
	PUBLIC PROPERTY ActivePage AS INT32
		GET
			RETURN SELF:SelectedIndex
		END GET
		SET
			IF VALUE >= 0 .AND. VALUE < SELF:PageCount
				SELF:SelectedIndex := VALUE
				SELF:_activePage := VALUE
			ENDIF
		END SET
	END PROPERTY

	/// <summary>
	/// Gets the read-only list of pages
	/// </summary>
	PUBLIC PROPERTY Pages AS IReadOnlyList<TabPage>
		GET
			RETURN SELF:TabPages:Cast<TabPage>():ToList():AsReadOnly()
		END GET
	END PROPERTY

	/// <summary>
	/// Gets the currently active page object
	/// </summary>
	PUBLIC PROPERTY CurrentPage AS Page
		GET
			IF SELF:SelectedTab IS Page
				RETURN (Page)SELF:SelectedTab
			ENDIF
			RETURN NULL
		END GET
	END PROPERTY

	/// <summary>
	/// Constructor - initializes the PageFrame
	/// </summary>
	PUBLIC CONSTRUCTOR()
		SUPER()
		SELF:_pages := List<Page>{}
		SELF:_pageCount := 0
		SELF:_activePage := -1
		SELF:_objectsCache := Dictionary<STRING, OBJECT>{}
		SELF:_baseClass := "PageFrame"
		SELF:_class := "PageFrame"
		SELF:_classLibrary := ""
		SELF:_comment := ""
		SELF:_helpContextID := 0
		SELF:_whatsThisHelpID := 0
		SELF:Alignment := TabAlignment.Top
		SELF:Dock := DockStyle.Fill
	END CONSTRUCTOR



	/// <summary>
	/// Adds a new page to the frame
	/// </summary>
	PUBLIC METHOD AddPage(cPageName AS STRING) AS Page
		LOCAL page AS Page
		page := Page{cPageName}
		page:PageIndex := SELF:PageCount
		SELF:_pages:Add(page)
		SELF:TabPages:Add(page)
		SELF:_pageCount := SELF:PageCount
		RETURN page
	END METHOD

	/// <summary>
	/// Adds a new page with caption and returns the page object
	/// </summary>
	PUBLIC METHOD AddPage(cPageName AS STRING, cCaption AS STRING) AS Page
		LOCAL page AS Page
		page := SELF:AddPage(cPageName)
		page:Caption := cCaption
		RETURN page
	END METHOD

	/// <summary>
	/// Removes a page at the specified index
	/// </summary>
	PUBLIC METHOD RemovePage(nIndex AS INT32) AS VOID
		IF nIndex >= 0 .AND. nIndex < SELF:PageCount
			SELF:TabPages:RemoveAt(nIndex)
			IF nIndex < SELF:_pages:Count
				SELF:_pages:RemoveAt(nIndex)
			ENDIF
			SELF:_pageCount := SELF:PageCount
		ENDIF
	END METHOD

	/// <summary>
	/// Gets a page by index
	/// </summary>
	PUBLIC METHOD GetPage(nIndex AS INT32) AS Page
		IF nIndex >= 0 .AND. nIndex < SELF:PageCount
			RETURN (Page)SELF:TabPages[nIndex]
		ENDIF
		RETURN NULL
	END METHOD

	/// <summary>
	/// Gets a page by name
	/// </summary>
	PUBLIC METHOD GetPageByName(cPageName AS STRING) AS Page
		FOREACH page AS TabPage IN SELF:TabPages
			IF page:Name == cPageName
				RETURN (Page)page
			ENDIF
		NEXT
		RETURN NULL
	END METHOD

	/// <summary>
	/// Gets an object by name from the object cache
	/// </summary>
	PUBLIC METHOD GetObject(cObjectName AS STRING) AS OBJECT
		IF SELF:_objectsCache:ContainsKey(cObjectName)
			RETURN SELF:_objectsCache[cObjectName]
		ENDIF
		RETURN NULL
	END METHOD

	/// <summary>
	/// Activates a specific page by index
	/// </summary>
	PUBLIC METHOD ActivatePage(nIndex AS INT32) AS VOID
		IF nIndex >= 0 .AND. nIndex < SELF:PageCount
			SELF:SelectedIndex := nIndex
			SELF:_activePage := nIndex
		ENDIF
	END METHOD

	/// <summary>
	/// Gets the index of the first page with the specified name
	/// </summary>
	PUBLIC METHOD GetPageIndex(cPageName AS STRING) AS INT32
		LOCAL i AS INT32
		FOR i := 0 TO SELF:PageCount - 1
			IF SELF:TabPages[i]:Name == cPageName
				RETURN i
			ENDIF
		NEXT
		RETURN -1
	END METHOD

	/// <summary>
	/// Releases all pages and clears the frame
	/// </summary>
	PUBLIC METHOD Release() AS VOID
		LOCAL i AS INT32
		FOR i := SELF:PageCount - 1 DOWNTO 0
			LOCAL page AS Page
			page := (Page)SELF:TabPages[i]
			IF page != NULL
				page:Dispose()
			ENDIF
		NEXT
		SELF:TabPages:Clear()
		SELF:_pages:Clear()
		SELF:_objectsCache:Clear()
		SELF:_pageCount := 0
	END METHOD

	/// <summary>
	/// Creates an instance of a class by name (helper method)
	/// </summary>
	PRIVATE METHOD CreateInstance(cClassName AS STRING) AS OBJECT
		LOCAL oType AS System.Type
		TRY
			RETURN CreateInstance( cClassName)
		CATCH
			// Return NULL if type cannot be created
            NOP
		END TRY
		RETURN NULL
	END METHOD

END CLASS

END NAMESPACE

