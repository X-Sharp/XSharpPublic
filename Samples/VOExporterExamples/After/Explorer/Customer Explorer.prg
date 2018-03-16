class CustomerExplorer inherit ExplorerWindow
	protect oCustomerServer	as CustomerServer
	protect oOrdersServer	as OrdersServer


method InitData() 

	// create, initialize, and set the relation on the data servers
	oCustomerServer	:= CustomerServer{}
	oOrdersServer	:= OrdersServer{}
	oCustomerServer:SetSelectiveRelation(oOrdersServer, #CustNum)
	
	// build list view columns and tree view items; it is unnecessary
	// to build list view items until a tree view item has been selected
	self:BuildListViewColumns()
	self:BuildTreeViewItems()
	
	return nil


method ViewSmallIcon() 
	return self:ListView:ViewAs(#SmallIconView)


method BuildListViewItems(uValue) 
	local oListView			as ListView
	local oListViewItem		as ListViewItem
	local dwCount			as dword
	
	// store list view locally for faster access
	oListView := self:ListView
	
	// position the customer server on the specified value
	oCustomerServer:Seek(uValue)

	// create child tree view items for all
	// child records that satisfy the relation		
	do while !oOrdersServer:EOF
		oListViewItem := ListViewItem{}

		// for each field, set the value in the item
		for dwCount := 1 to oOrdersServer:FCount
			oListViewItem:SetValue(oOrdersServer:FieldGet(dwCount), oOrdersServer:FieldSym(dwCount))
		next dwCount
		oListViewItem:ImageIndex := 1
		oListView:AddItem(oListViewItem)
		
		oOrdersServer:Skip()
	end do

	return nil


method BuildTreeViewItems() 
	local oTreeView			as TreeView
	local oTreeViewItem		as TreeViewItem
	local symParentItem		as symbol
	
	// store tree view locally for faster access
	oTreeView := self:TreeView
	
	// add the master tree view item
	oTreeViewItem			:= TreeViewItem{}
	oTreeViewItem:NameSym	:= #Customer_Master
	oTreeViewItem:TextValue	:= "Customers"
	oTreeViewItem:Bold		:= true
	oTreeView:AddItem(#Root, oTreeViewItem)
	
	// reposition the customer server to the topmost record
	oCustomerServer:GoTop()
	
	// create each tree view item from a record in the
	// customer server and add it to the tree view
	do while !oCustomerServer:EOF
		oTreeViewItem := TreeViewItem{}
		symParentItem				:= String2Symbol("Customers_" + AllTrim(AsString(oCustomerServer:CustNum)))
		oTreeViewItem:NameSym		:= symParentItem
		oTreeViewItem:Value			:= oCustomerServer:LastName
		oTreeViewItem:ImageIndex	:= 2
		oTreeViewItem:SelectedImageIndex := 1
		oTreeView:AddItem(#Customer_Master, oTreeViewItem)

		// create child tree view items for all
		// child records that satisfy the relation		
		do while !oOrdersServer:EOF
			oTreeViewItem := TreeViewItem{}
			oTreeViewItem:NameSym		:= String2Symbol("Orders_" + AllTrim(AsString(oOrdersServer:OrderNum)))
			oTreeViewItem:Value			:= oOrdersServer:OrderNum
			oTreeViewItem:ImageIndex	:= 1
			oTreeView:AddItem(symParentItem, oTreeViewItem)
			
			oOrdersServer:Skip()
		end do
	
		oCustomerServer:Skip()
	end do

	return nil


CONSTRUCTOR(oOwner) 
	local oDimension	as Dimension
	local oImageList	as ImageList
	local oTreeView		as TreeView
	local oListView		as ListView
	
	// initialize super class with no labels, and
	// the specified view control subclasses
	SUPER(oOwner, false, #CustomerTreeView, #CustomerListView)
	
	// store the view controls locally for faster access
	oTreeView := self:TreeView
	oListView := self:ListView
	
	// initialize menu and caption
	self:Menu := CustomerExplorerMenu{self}
	self:Caption := "Exploring Customer Orders"
	self:Icon := IconTwo{}
	self:IconSm := IconTwo{}

	// initialize components of the list view
	oListView:ViewAs(#ReportView)
	oListView:EnableSort(#SortOrders)
	oListView:ContextMenu := OrdersListViewMenu{}

	// create the icon objects for the image lists
	oImageList := ImageList{2, Dimension{20, 20}}
	oImageList:Add(IconOne{})
	oImageList:Add(IconTwo{})
	oTreeView:ImageList := oImageList
	oListView:SmallImageList := oImageList
	oImageList := ImageList{2, Dimension{32, 32}}
	oImageList:Add(IconOne{})
	oImageList:Add(IconTwo{})
	oListView:LargeImageList := oImageList
	
	// initialize pane sizes
	oDimension := self:GetPaneSize(1)
	oDimension:Width := oDimension:Width / 2
	self:SetPaneSize(oDimension, 1)

	// initialize data
	self:InitData()
	
	return self


method ViewIcon() 
	return self:ListView:ViewAs(#IconView)


method ViewList() 
	return self:ListView:ViewAs(#ListView)


method ViewReport() 
	return self:ListView:ViewAs(#ReportView)


method BuildListViewColumns() 
	local oListView			as ListView
	local oListViewColumn	as ListViewColumn
	local dwCount			as dword
	
	// store list view locally for faster access
	oListView := self:ListView
	
	// for each field in the orders server, create a
	// list view column and add it to the list view
	for dwCount := 1 to oOrdersServer:FCount
		oListViewColumn			:= ListViewColumn{}
		oListViewColumn:NameSym	:= oOrdersServer:FieldSym(dwCount)
		oListViewColumn:Caption	:= oOrdersServer:FieldName(dwCount)
		oListViewColumn:Width	:= oOrdersServer:FieldSpec(dwCount):Length
		oListView:AddColumn(oListViewColumn)
	next dwCount

	return nil


method DeleteListViewItems() 
	return self:ListView:DeleteAll()


method TreeViewSelectionChanged(oTreeViewSelectionEvent) 
	local oTreeViewItem	as TreeViewItem
	
	super:TreeViewSelectionChanged(oTreeViewSelectionEvent)
	
	oTreeViewItem := oTreeViewSelectionEvent:NewTreeViewItem
	if oTreeViewItem != NULL_OBJECT .and. InStr("CUSTOMERS_", Upper(Symbol2String(oTreeViewItem:NameSym)))
		// left-click occurred on a customer tree view item
		self:DeleteListViewItems()
		self:BuildListViewItems(oTreeViewItem:Value)
	endif

	return nil


method ListViewColumnClick(oListViewClickEvent) 
	local symColumnName as symbol
	
	super:ListViewColumnClick(oListViewClickEvent)

	symColumnName := oListViewClickEvent:ListViewColumn:NameSym
	
	// based on the column name, set the appropriate sort routine name
	do case
		case symColumnName == #CUSTNUM
			self:ListView:EnableSort(#SortByCustNum)

		case symColumnName == #ORDERNUM
			self:ListView:EnableSort(#SortByOrderNum)

		case symColumnName == #ORDER_DATE
			self:ListView:EnableSort(#SortByOrderDate)

		case symColumnName == #SHIP_DATE
			self:ListView:EnableSort(#SortByShippingDate)

		case symColumnName == #SHIP_ADDRS
			self:ListView:EnableSort(#SortByShippingAddress)

		case symColumnName == #SHIP_CITY
			self:ListView:EnableSort(#SortByShippingCity)

		case symColumnName == #SHIP_STATE
			self:ListView:EnableSort(#SortByShippingState)

		case symColumnName == #SHIP_ZIP
			self:ListView:EnableSort(#SortByShippingZipCode)

		case symColumnName == #ORDERPRICE
			self:ListView:EnableSort(#SortByOrderPrice)

		case symColumnName == #SELLER_ID
			self:ListView:EnableSort(#SortBySellerID)
	end case

	// execute the sort
	self:ListView:SortItems()

	return nil


METHOD Close(oCloseEvent) 
	
	// clean up the open data servers
	IF oCustomerServer != NULL_OBJECT
		oCustomerServer:Close()
		oCustomerServer := NULL_OBJECT
	ENDIF
	IF oOrdersServer != NULL_OBJECT
		oOrdersServer:Close()
		oOrdersServer := NULL_OBJECT
	ENDIF

	// force garbage collection
	CollectForced()

	RETURN SUPER:Close(oCloseEvent)


method EditOrder() 
	local cTitle	as string
	local cMessage	as string
	
	cTitle		:= "Option Not Available"
	cMessage	:= "This option has not yet been implemented"

	TextBox{self:Owner, cTitle, cMessage}:Show()
	
	return nil



method CloseExplorer() 
	
	return self:EndWindow()


method Refresh() 
	// clear and rebuild the list view items
	self:DeleteListViewItems()
	self:BuildListViewItems(oCustomerServer:LastName)
	RETURN SELF



END CLASS
class CustomerListView inherit ListView


method SortByCustNum(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#CUSTNUM)
    uValue2 := oListViewItem2:GetValue(#CUSTNUM)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByOrderNum(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#ORDERNUM)
    uValue2 := oListViewItem2:GetValue(#ORDERNUM)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByOrderDate(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#ORDER_DATE)
    uValue2 := oListViewItem2:GetValue(#ORDER_DATE)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByShippingDate(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SHIP_DATE)
    uValue2 := oListViewItem2:GetValue(#SHIP_DATE)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByShippingAddress(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SHIP_ADDRS)
    uValue2 := oListViewItem2:GetValue(#SHIP_ADDRS)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByShippingCity(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SHIP_CITY)
    uValue2 := oListViewItem2:GetValue(#SHIP_CITY)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByShippingState(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SHIP_STATE)
    uValue2 := oListViewItem2:GetValue(#SHIP_STATE)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByShippingZipCode(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SHIP_ZIP)
    uValue2 := oListViewItem2:GetValue(#SHIP_ZIP)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortByOrderPrice(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#ORDERPRICE)
    uValue2 := oListViewItem2:GetValue(#ORDERPRICE)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


method SortBySellerID(oListViewItem1, oListViewItem2) 
	local uValue1	as usual
	local uValue2	as usual
	
    uValue1 := oListViewItem1:GetValue(#SELLER_ID)
    uValue2 := oListViewItem2:GetValue(#SELLER_ID)

    if uValue1 > uValue2
    	return -1
    elseif uValue1 < uValue2
    	return 1
    endif

	return 0


END CLASS
class CustomerTreeView inherit TreeView
END CLASS
