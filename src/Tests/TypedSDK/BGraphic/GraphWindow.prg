#region DEFINES
define MAX_COLORS := 6
#endregion

CLASS ChartTypeBox INHERIT ComboBoxEx


METHOD Dispatch(oEvent AS Event)
	local GraphWnd as GraphChildWindow

	if (oEvent:uMsg == WM_COMMAND)  .and. (HiWord(oEvent:wParam) == CBN_SELCHANGE)
		GraphWnd := self:Owner:Owner
		GraphWnd:Type := self:GetItemValue(self:CurrentItemNo)
		GraphWnd:RePaint()
	endif

	return super:Dispatch(oEvent)


END CLASS
class GraphChildWindow inherit ChildAppWindow
	protect aData as array
	protect oBlackPen as Pen
	protect aBrushes  as array
	protect oArial10 as Font
	protect symCurrentType as symbol
	protect fMaxData as float
	protect fSumData as float
	protect oCBX as ChartTypeBox


ACCESS Data
	return aData


ASSIGN Data(aNewData)
	local i, iLen as DWORD

	aData := aNewData

	iLen := ALen(aData)
	fMaxData := fSumData := 0.0
	for i:= 1 to iLen
		fMaxData := Max(fMaxData, aData[i][1])
		fSumData += aData[i][1]
	next

	self:Menu:UnCheckItem(IDM_GraphMenu_Data_Sample_Set_1_ID)
	self:Menu:UnCheckItem(IDM_GraphMenu_Data_Sample_Set_2_ID)
	self:Menu:UnCheckItem(IDM_GraphMenu_Data_Sample_Set_3_ID)

	self:Toolbar:UnPressItem(IDM_GraphMenu_Data_Sample_Set_1_ID)
	self:Toolbar:UnPressItem(IDM_GraphMenu_Data_Sample_Set_2_ID)
	self:Toolbar:UnPressItem(IDM_GraphMenu_Data_Sample_Set_3_ID)

	if (aNewData == gaSampleData1)
		self:Menu:CheckItem(IDM_GraphMenu_Data_Sample_Set_1_ID)
		self:Toolbar:PressItem(IDM_GraphMenu_Data_Sample_Set_1_ID)
	elseif (aNewData == gaSampleData2)
		self:Menu:CheckItem(IDM_GraphMenu_Data_Sample_Set_2_ID)
		self:Toolbar:PressItem(IDM_GraphMenu_Data_Sample_Set_2_ID)
	elseif (aNewData == gaSampleData3)
		self:Menu:CheckItem(IDM_GraphMenu_Data_Sample_Set_3_ID)
		self:Toolbar:PressItem(IDM_GraphMenu_Data_Sample_Set_3_ID)
	endif

	return aData


METHOD DrawBar()
	local oSize as Dimension
	local iDatas, i as DWORD
	local iStep, iY, iX, iCanvasY as DWORD

	if Empty(aData)
		   return self
	endif

	iDatas   := ALen(aData)
	oSize    := self:CanvasArea:Size
	iCanvasY := oSize:Height - self:ToolBar:Size:Height

	iStep  := (iCanvasY / iDatas) / 3
	iY     := iStep / 2
	for i := 1 to iDatas
		iX := ((oSize:Width - 45) * aData[i][1]) / fMaxData
		self:Draw(RectangleObject{Point{10, iY}, Dimension{iX, iStep * 2}, oBlackPen, aBrushes[MOD(i-1, MAX_COLORS)+1]})
		self:Draw(TextObject{Point{12, iY+2}, aData[i][2], oArial10})
		self:Draw(TextObject{Point{iX + 12, iY+2}, NTrim(aData[i][1]), oArial10 })
		iY += iStep * 3
	NEXT


   return self

METHOD DrawColumn()
	local oSize as Dimension
	local iDatas, i as DWORD
	local iStep, iY, iX, iCanvasY as DWORD

	if Empty(aData)
		   return self
	endif

	iDatas   := ALen(aData)
	oSize    := self:CanvasArea:Size
	iCanvasY := oSize:Height - self:ToolBar:Size:Height

	iStep  := ((DWORD(oSize:Width) / iDatas) / 3)
	iX     := iStep / 2
	for i := 1 to iDatas
		iY := ((iCanvasY - 40) * aData[i][1]) / fMaxData
		self:Draw(RectangleObject{Point{iX, 30}, Dimension{iStep * 2, iY}, oBlackPen, aBrushes[MOD(i-1, MAX_COLORS)+1]})
		self:Draw(TextObject{Point{iX, 10}, aData[i][2], oArial10})
		self:Draw(TextObject{Point{iX+2, 10 + iY}, NTrim(aData[i][1]), oArial10})
		iX += iStep * 3
	NEXT

   return self

METHOD DrawPie()
	local oSize as Dimension
	local iDatas, i as DWORD
	local oMidPt as Point
	local fAccData := 0.0, fAngle, fOldAngle, fTextAngle, fTextRad as float

	if Empty(aData)
		   return self
	endif

	iDatas   := ALen(aData)
	oSize    := self:CanvasArea:Size
	oSize:Height -= self:ToolBar:Size:Height

	oMidPt := Point{oSize:Width / 2, oSize:Height / 2}
	for i := 1 to iDatas
		fAccData += aData[i][1]
		fOldAngle  := fAngle
		fAngle     := (360.0 * fAccData) / fSumData
		self:Draw(PieObject{Point{0,0}, oSize, oBlackPen, aBrushes[MOD(i-1, MAX_COLORS)+1], fOldAngle, fAngle})
		fTextAngle := fOldAngle  + ((fAngle - fOldAngle) / 2.0)
		fTextRad   := Min(oSize:Width, oSize:Height) / 2.5
		self:Draw(TextObject{Point{oMidPt:X + (Sin(fTextAngle * PI / 180.0) * fTextRad), oMidPt:Y  + (Cos(fTextAngle * PI / 180.0) * fTextRad)}, aData[i][2], oArial10})
	NEXT


   return self

METHOD Expose(oExposeEvent AS ExposeEvent)
	super:Expose(oExposeEvent)

	Send(self, symCurrentType)

	return nil


CONSTRUCTOR(oParent, aData, symType)
	local oIL as ImageList

	SUPER(oParent, true)

	Default(@symType, #DrawColumn)

	self:Caption := "Business Graphic Sample Data"
	self:Menu := GraphMenu{}
	if IsArray(aData)
		self:Data := aData
	ENDIF

	oBlackPen := Pen{Color{COLORBLACK}}

	aBrushes := ArrayCreate(MAX_COLORS)
	aBrushes[1] := Brush{Color{COLORBLUE}}
	aBrushes[2] := Brush{Color{COLORRED}}
	aBrushes[3] := Brush{Color{COLORGREEN}}
	aBrushes[4] := Brush{Color{COLORYELLOW}}
	aBrushes[5] := Brush{Color{COLORMAGENTA}}
	aBrushes[6] := Brush{Color{COLORCYAN}}

	oArial10  := Font{, 10, "Arial"}

	oCBX := ChartTypeBox{self:Toolbar, 200, Point{0, 0}, Dimension{50, 125}, BOXDROPDOWNLIST}
	oIL := ImageList{100, Dimension{16, 16}}
	oIL:Add(Bitmap{ResourceID{"IDB_DEFTOOLBAR"}, BMP_3DTRANSPARENT})
	oCBX:ImageList := oIL

	oCBX:AddItem("Column Chart",,#DRAWCOLUMN,IDT_COLUMNCHART)
	oCBX:AddItem("Bar Chart",,#DRAWBAR,IDT_BARCHART)
	oCBX:AddItem("Pie Chart",,#DRAWPIE,IDT_PIECHART)

	self:ToolBar:AddBand(#CHARTTYPE, oCBX, -1, 50, 25, "Chart Type:")

	self:Type := symType

	return self


METHOD MenuCommand(oMenuEvent AS MenuCommandEvent)

	if (oMenuEvent:NameSym == #ChangeData)
		do case
		case (oMenuEvent:ItemID == IDM_GraphMenu_Data_Sample_Set_1_ID)
			self:Data := gaSampleData1
		case (oMenuEvent:ItemID == IDM_GraphMenu_Data_Sample_Set_2_ID)
			self:Data := gaSampleData2
		case (oMenuEvent:ItemID == IDM_GraphMenu_Data_Sample_Set_3_ID)
			self:Data := gaSampleData3
		ENDCASE

		self:Repaint()

	elseif (oMenuEvent:NameSym == #ChangeType)
		do case
		case (oMenuEvent:ItemID == IDM_GraphMenu_Type_Column_ID)
			self:Type := #DrawColumn
		case (oMenuEvent:ItemID == IDM_GraphMenu_Type_Bar_ID)
			self:Type := #DrawBar
		case (oMenuEvent:ItemID == IDM_GraphMenu_Type_Pie_ID)
			self:Type := #DrawPie
		ENDCASE

		self:Repaint()
	ENDIF

	return super:MenuCommand(oMenuEvent)


ACCESS Type
	return symCurrentType


ASSIGN TYPE(symNewType)
	self:Menu:UnCheckItem(IDM_GraphMenu_Type_Column_ID)
	self:Menu:UnCheckItem(IDM_GraphMenu_Type_Bar_ID)
	self:Menu:UnCheckItem(IDM_GraphMenu_Type_Pie_ID)

	do case
	case (symNewType == #DrawColumn)
		self:Menu:CheckItem(IDM_GraphMenu_Type_Column_ID)
	case (symNewType == #DrawBar)
		self:Menu:CheckItem(IDM_GraphMenu_Type_Bar_ID)
	case (symNewType == #DrawPie)
		self:Menu:CheckItem(IDM_GraphMenu_Type_Pie_ID)
	ENDCASE

	oCBX:Value := symNewType

	return (symCurrentType := symNewType)



END CLASS
