CLASS DataExplorer INHERIT ExplorerWindow
	PROTECT oDataTreeView AS TreeView
	PROTECT lShow AS LOGIC
	EXPORT aServerInfo, aLevels AS ARRAY
	PROTECT aTreeViewValues AS ARRAY


ACCESS DataTreeView
	RETURN oDataTreeView

ASSIGN DataTreeView(oDTV)
	SELF:oDataTreeView := oDTV
	RETURN SELF:oDataTreeView


METHOD DeleteLVRecord()
   LOCAL i AS DWORD
   LOCAL x AS USUAL
   LOCAL n, nNextRec AS LONG

   IF SELF:ListView:SelectedCount > 0

       // get record number of next record
       // so we can position on that record later
       SELF:Listview:Server:SuspendNotification()
       n := SELF:ListView:Server:Recno
       SELF:ListView:Server:Skip(1)
       IF ! SELF:ListView:Server:EOF
           nNextRec := SELF:ListView:Server:Recno
       ENDIF
       SELF:ListView:Server:Goto(n)

       // empty the record, so it moves out of scope
       FOR i := 1 TO SELF:Listview:Server:Fcount
           x := SELF:Listview:Server:FIELDGET(i)
           DO CASE
           CASE IsString(x)
               x:=NULL_STRING
           CASE IsDate(x)
               x:=NULL_DATE
           CASE IsLogic(x)
               x:=FALSE
           CASE IsNumeric(x)
               x:=0
           ENDCASE
           SELF:Listview:Server:FIELDPUT(i,x)
       NEXT
       SELF:Listview:Server:ResetNotification()

       // delete the selected record in the listview server
       SELF:Listview:Server:Delete()
       IF nNextRec == 0
           SELF:Listview:Server:GoBottom()
       ELSE
           SELF:Listview:Server:Goto(nNextRec)
       ENDIF

       // refresh the DataListview
       SELF:ListView:Refresh()
   ENDIF
	RETURN SELF


METHOD EditLVRecord()
	LOCAL oNewDialog AS DataDialog

	IF SELF:ListView:SelectedCount > 0

		// create new formview dialog to edit the record
	  oNewDialog := StdDataDialog{SELF, SELF:Listview:Server}
	  oNewDialog:Caption := "Edit "+ SELF:Listview:Server:Name
	  oNewDialog:ContextMenu := EditMenu{}
	  oNewDialog:ViewAs(#FormView)
	 	oNewDialog:Show()

	ENDIF
	RETURN SELF

METHOD FormView
	LOCAL oNewDialog AS DataDialog

	// craete a DataDialog to edit data
	oNewDialog := StdDataDialog{SELF, SELF:ListView:Server}
	oNewDialog:ViewAs(#FormView)
  oNewDialog:Show()
	RETURN SELF

CONSTRUCTOR(oOwner, aServers, aTVValues, lShowFields )
  LOCAL oSmallImageList AS ImageList
*  LOCAL oLVServer AS DBServer

  SUPER(oOwner, FALSE, #DATATREEVIEW, #DATALISTVIEW)

	// Set initial variables
	SELF:aServerInfo := aServers
	SELF:aTreeViewValues := aTVValues
	SELF:ListView:ContextMenu := DataLVContext{SELF}

	// Set the Showfields flag
	SELF:ShowFields := lShowFields

	// create the icon objects for the imagelists
	oSmallImageList := ImageList{2, Dimension{20, 20}}
	oSmallImageList:Add(iconDB{})
	oSmallImageList:Add(IconDBSelect{})
	oSmallImageList:Add(iconRecord{})
	oSmallImageList:Add(IconRecordSelect{})

	// create treeview imagelist
	SELF:TreeView:ImageList := oSmallImageList

	SELF:QuitOnClose := TRUE

	// assign server for treeview
	IF !IsNil(SELF:aServerInfo[TVSERVER])
	  SELF:TreeView:Server := CreateInstance(aServerInfo[TVSERVER])
      SELF:Treeview:Server:RegisterClient( SELF )
  ENDIF

	// assign server for listview
	IF !IsNil(SELF:aServerInfo[LVSERVER])
	  SELF:ListView:Server := CreateInstance(aServerInfo[LVSERVER])
        SELF:Listview:Server:SetFilter({||!DELETED()},"!DELETED()")
	ENDIF

  // fill data in treeview
  SELF:aLevels := {{SELF:Treeview:server:NameSym, aTreeviewValues} }
  SELF:TreeView:InitData(aLevels, SELF:ShowFields, aServerInfo[RELFIELD] )

  // Show the TreeView expanded
	SELF:TreeView:Expand(SELF:TreeView:GetFirstVisibleItem())

	// Select the first ChildItem
	SELF:TreeView:SelectItem(SELF:TreeView:GetFirstChildItem())

  RETURN SELF


METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent)
	SUPER:ListViewMouseButtonDoubleClick(oListViewMouseEvent)
	//Put your changes here
  SELF:EditLVRecord()
	RETURN NIL


METHOD NewLVRecord()
	LOCAL oNewDialog AS DataDialog

	// create a new record
	SELF:Listview:Server:Append()

	// fill the relationfield of the new created record
	SELF:Listview:Server:FIELDPUT(SELF:aServerInfo[RELFIELD],SELF:TreeView:Server:FIELDGET(SELF:aServerInfo[RELFIELD] ))

	// notify the ListView about the append
  SELF:ListView:Notify(NOTIFYAPPEND)

	// create new formview dialog to edit the record
  oNewDialog := StdDataDialog{SELF, SELF:Listview:Server}
  oNewDialog:Caption := "Edit "+ SELF:Listview:Server:Name
  oNewDialog:ContextMenu := EditMenu{}
  oNewDialog:ViewAs(#FormView)
 	oNewDialog:Show()
	RETURN SELF


METHOD Notify(kNotification, uDescription)
*  LOCAL oItem AS TreeViewItem
	LOCAL x AS USUAL

//   VTrace VMethod
  DO CASE

  CASE (kNotification == NOTIFYRECORDCHANGE)  .OR. (kNotification == NOTIFYGOBOTTOM)  .OR. ;
    (kNotification == NOTIFYGOTOP)  .OR. (kNotification == NOTIFYDELETE)  .OR. (kNotification == NOTIFYAPPEND)
    // record position has changed

		x := SELF:TreeView:Server:FIELDGET(SELF:aServerInfo[RELFIELD] )

		// build the relation manual
	  SELF:Listview:Server:OrderScope(TOPSCOPE,x)
  	SELF:ListView:Server:OrderScope(BOTTOMSCOPE,x)



	OTHERWISE
			RETURN TRUE
  ENDCASE

  RETURN SELF

METHOD SetShowFields

	/*
 	depending OF the menuselection configure the treeview,
	to show all record information within the tree
	*/

	// switch the menu- and toolbar items
  IF SELF:Showfields == TRUE
  	SELF:ShowFields := FALSE
  	SELF:Menu:UnCheckItem(IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID)
  	SELF:ToolBar:UnPressItem(IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID)
  ELSE
  	SELF:ShowFields := TRUE
  	SELF:Menu:CheckItem(IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID)
  	SELF:ToolBar:PressItem(IDM_DataExplorerMenu_View_Show_all_Fields_in_Treeview_ID)
  ENDIF

	// refresh the treeview
	SELF:TreeView:DeleteAll()
	SELF:TreeView:InitData(aLevels, SELF:ShowFields, aServerInfo[RELFIELD] )
	SELF:TreeView:Expand(aLevels[1][1])
	SELF:TreeView:SelectItem(aLevels[1][1])
	RETURN SELF

ACCESS ShowFields
	RETURN SELF:lShow

ASSIGN ShowFields(uValue)
	SELF:lShow := uValue
	RETURN SELF:lShow

METHOD TreeViewKeyDown(oTreeViewKeyEvent)
	SUPER:TreeViewKeyDown(oTreeViewKeyEvent)
	//Put your changes here

	DO CASE
		CASE oTreeViewKeyEvent:KeyCode == KEYENTER
			// SELF:EditLVRecord(SELF:Treeview:Server)

	OTHERWISE

	ENDCASE
	RETURN NIL


METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent)

	SUPER:TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent)

	//Put your changes here

	RETURN NIL


METHOD TreeViewSelectionChanged(oTVSEvent)
	LOCAL cSelected AS STRING
	LOCAL nPos AS DWORD


  cSelected := Symbol2String(oTVSEvent:NewTreeViewItem:NameSym)

  // check value of the new treeview item
  IF !IsNil(oTVSEvent:NewTreeViewItem:Value)
		nPos := oTVSEvent:NewTreeViewItem:Value
	ELSE
		nPos := 0
	ENDIF

	// call super event method
	SUPER:TreeViewSelectionChanged(oTVSEvent)

	// set the record pointer
    SELF:TreeView:Server:Goto(nPos)

	RETURN SELF


END CLASS
CLASS StdDataDialog INHERIT DataDialog
	PROTECT lAppend AS LOGIC

METHOD CloseWindow()

	// save the changes to the server
	SELF:Server:Commit()

	// notify the owner window for a screen refresh
	SELF:Owner:Notify()

	SELF:EndWindow()
	RETURN SELF

CONSTRUCTOR(oParentWindow, oServer)
	LOCAL sCaption AS STRING

	SUPER(oParentWindow)

	SELF:Menu := EditMenu{SELF}

	sCaption  := "Edit Record: "

	IF !IsNil(oServer)
		SELF:Use(oServer)
	ENDIF

	SELF:PostInit()
	RETURN SELF

METHOD PostInit()
	LOCAL i AS DWORD

	// disable the relation field to avoid changes
	FOR i := 1 TO Len(aControls)

		IF SELF:aControls[i]:NameSym == SELF:Owner:aServerInfo[RELFIELD]
			aControls[i]:disable()
		ENDIF
	NEXT
	RETURN SELF



END CLASS
