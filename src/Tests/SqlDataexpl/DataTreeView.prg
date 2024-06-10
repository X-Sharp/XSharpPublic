CLASS DataTreeView INHERIT TreeView
	PROTECT odbServer AS dbServer
	PROTECT symRelationField AS SYMBOL


METHOD AddDataItem(oTVItem,symCurrentLevel,aNewLevel,nPos, lShowFields) 
	LOCAL cItemText, cFieldItemText AS STRING
	LOCAL i, nFields AS DWORD
	
	FOR i := 1 TO Len(aNewLevel)
		cItemText := cItemText+AllTrim(SELF:Server:FIELDGET(aNewLevel[i]))
		IF i < Len(aNewLevel)
			cItemText+=", "
		ENDIF
	NEXT

	oTVItem:NameSym := String2Symbol(cItemText)
	oTVItem:TextValue := cItemText
	oTVItem:Bold := TRUE
	oTVItem:Value := nPos

	SELF:AddItem(symCurrentLevel,oTVItem)
	
	IF lShowFields == TRUE
		nFields := SELF:server:Fcount
		FOR i := 1 TO nFields
			cFieldItemText := SELF:server:FieldName(i)+": "+AsString(SELF:server:FIELDGET(i))
			oTVItem:NameSym := String2Symbol(cFieldItemText)
			oTVItem:TextValue := cFieldItemText
			oTVItem:Bold := FALSE
			oTVItem:Value := nPos
			SELF:AddItem(String2Symbol(cItemText),oTVItem)
		NEXT
	ENDIF
	
	RETURN SELF


METHOD FocusChange(oFocusChangeEvent) 
	LOCAL lGotFocus AS LOGIC
	lGotFocus := IIf(oFocusChangeEvent == NULL_OBJECT, FALSE, oFocusChangeEvent:GotFocus)
	SUPER:FocusChange(oFocusChangeEvent)
	//Put your changes here
	
	DO CASE
		CASE lGotFocus == TRUE

			SELF:Owner:Menu:DisableItem(IDM_DataExplorerMenu_Edit_Orders_New_Order_ID)
			SELF:Owner:Menu:DisableItem(IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID)
			SELF:Owner:Menu:DisableItem(IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID)	
			
		CASE lGotFocus == FALSE
			
			SELF:Owner:Menu:EnableItem(IDM_DataExplorerMenu_Edit_Orders_New_Order_ID)
			SELF:Owner:Menu:EnableItem(IDM_DataExplorerMenu_Edit_Orders_Edit_Order_ID)	
			SELF:Owner:Menu:EnableItem(IDM_DataExplorerMenu_Edit_Orders_Delete_Order_ID)	
			
	ENDCASE			
	
	RETURN NIL
	

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	
	SUPER(oOwner, xID, oPoint, oDimension, kStyle)
	
	SELF:SetStyle(TVS_EDITLABELS,FALSE)
	RETURN SELF

METHOD InitData(aLevelSym, lShowFields, symRelField) 
	LOCAL oTVItem AS TreeViewItem
*	LOCAL cItemText AS STRING
	LOCAL i, iLen, nPosFirst AS DWORD
	
	SELF:RelationField := symRelField
	
	SELF:server:SuspendNotification()
	
	// get number of Treeview Levels	
	iLen := Len(aLevelSym)	
		
	// create root item	
	oTVItem := TreeViewItem{}
	
	// set item value and properties
	oTVItem:NameSym := aLevelSym[1][1]
	oTVItem:TextValue := Symbol2String(aLevelSym[1][1] )
	oTVItem:Bold := TRUE
	oTVItem:ImageIndex	:= 1
	oTVItem:SelectedImageIndex := 2

	// add item to Treeview
	SELF:AddItem(#Root,oTVItem)
	
	SELF:Server:GoTop()

	FOR i:=1 TO iLen

		WHILE !SELF:Server:EOF
			oTVItem:ImageIndex	:= 3
			oTVItem:SelectedImageIndex := 4
			nPosFirst := SELF:Server:RecNo
			SELF:AddDataItem(oTVItem,aLevelSym[i][1],aLevelSym[i][2],nPosFirst, lShowFields)

			SELF:Server:Skip()
		ENDDO
	NEXT

	SELF:Server:GoTop()
	
 SELF:server:ResetNotification()
	RETURN SELF

ACCESS RelationField 
	RETURN SELF:symRelationField

ASSIGN RelationField(uValue) 
	SELF:symRelationField := uValue
	RETURN SELF:symRelationField


ACCESS Server 
	RETURN SELF:odbServer	

ASSIGN Server(oDataServer) 
	SELF:odbServer := oDataServer
	RETURN SELF:odbServer


END CLASS
