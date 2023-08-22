// 541. AccessViolationException problem with GC and PSZs

// exception cannot be reliably reproduced all the time, but this specific sample
// seems to always recreate it on my machine.
// Most likely the problem is related to C362, because the DIM array in DoTheTest() is not pinned
// Same code compiled in vulcan (which does pin the array) runs without errors

/*
System.AccessViolationException: Attempted to read or write protected memory. This is often an indication that other memory is corrupt.

   at VulcanVOWin32APILibrary.Functions.CallWindowProc(Void* lpPrevWndFunc, Void* hwnd, UInt32 Msg, UInt32 wParam, Int32 lParam)
   at VulcanVOGUIClasses.Functions.__WCControlProc(Void* hWnd, UInt32 umsg, UInt32 wParam, Int32 lParam)
   at VulcanVOWin32APILibrary.Functions.SendMessage(Void* hwnd, UInt32 Msg, UInt32 wParam, Int32 lParam)
   at VulcanVOWin32APILibrary.Functions.TreeView_GetItem(Void* hwnd, _winTV_ITEM* pitem)
   at TreeWin.DoTheTest(__Usual[] Xs$Args)
   at TreeWin.DoTest(__Usual[] Xs$Args)
*/

#pragma warnings(219, off) // assigned but not used

#define TREEWIN_DOTEST 100
#define TREEWIN_TREEVIEW1 101
#define IDM_Menu1 "Menu1"
#define IDM_Menu1_DoTest_ID 15001

[STAThreadAttribute];
FUNCTION Start( ) AS VOID
LOCAL oShell AS ShellWindow
LOCAL oApp AS App

oApp := App{}

oShell := ShellWindow{oApp}
oShell:Menu := Menu1{}
oShell:Show()

oApp:Exec()

PARTIAL CLASS TreeWin INHERIT DATAWINDOW
	PROTECT oCCDoTest AS PUSHBUTTON
	PROTECT oDCTreeView1 AS TREEVIEW

	// User code starts here (DO NOT remove this line)  ##USER##
	CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)

		SUPER(oWindow , ResourceID{"TreeWin" , _GetInst()},iCtlID)

		SELF:oCCDoTest := PUSHBUTTON{SELF , ResourceID{ TREEWIN_DOTEST  , _GetInst() } }
		SELF:oCCDoTest:HyperLabel := HyperLabel{#DoTest , "Press here to Do Test" , NULL_STRING , NULL_STRING}

		SELF:oDCTreeView1 := TREEVIEW{SELF , ResourceID{ TREEWIN_TREEVIEW1  , _GetInst() } }
		SELF:oDCTreeView1:HyperLabel := HyperLabel{#TreeView1 , NULL_STRING , NULL_STRING , NULL_STRING}

		SELF:Caption := "DataWindow Caption"
		SELF:HyperLabel := HyperLabel{#TreeWin , "DataWindow Caption" , NULL_STRING , NULL_STRING}
		IF !IsNil(oServer)
			SELF:Use(oServer)
		ENDIF


		SELF:PostInit(oWindow,iCtlID,oServer,uExtra)

	RETURN


	METHOD PostInit(oWindow,iCtlID,oServer,uExtra) CLASS TreeWin
		//Put your PostInit additions here
		SELF:oDCTreeView1:AddItem(#ROOT , TreeViewItem{#CHILD , "aaaa"})
		SELF:oDCTreeView1:AddItem(#CHILD , TreeViewItem{#CHILD1 , "bbb"})
	RETURN NIL


	METHOD DoTest( )
//		FOR LOCAL n := 1 AS INT UPTO 10000
		FOR LOCAL n := 1 AS INT UPTO 1000
			? n
			SELF:DoTheTest()
		NEXT
		? "This needs to be here"
	RETURN NIL
	METHOD DoTheTest( )
		LOCAL strucItem IS _winTV_Item
		LOCAL pszItemText AS PSZ
		LOCAL DIM aBuf[1000] AS BYTE

		strucItem:hItem := SELF:oDCTreeView1:__GetHandleFromSymbol(#CHILD1)
		strucItem:mask := 255
		pszItemText := @aBuf[1]
		strucItem:pszText := pszItemText
		strucItem:cchTextMax := 256

		#warning No exception without the collect
		GC.Collect()

		LOCAL u AS USUAL
		#warning exception here
		u := TreeView_GetItem(SELF:oDCTreeView1:Handle(),  @strucItem)
//		? u

	RETURN NIL


END CLASS

CLASS Menu1 INHERIT Menu

	CONSTRUCTOR( oOwner )

		SUPER( ResourceID { "Menu1" , _GetInst( ) } )

		SELF:RegisterItem(IDM_Menu1_DoTest_ID, ;
		HyperLabel{ #TreeWin , "Press HERE to DoTest" , NULL_STRING , NULL_STRING })

		SELF:PostInit()

	RETURN

END CLASS
