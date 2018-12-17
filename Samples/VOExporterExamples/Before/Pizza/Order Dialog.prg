#region DEFINES
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 104 
STATIC DEFINE HELPABOUT_THEFIXEDICON1 := 101 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT1 := 100 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT2 := 102 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT3 := 103 
STATIC DEFINE ORDERDIALOG_BROCCOLICB := 113 
STATIC DEFINE ORDERDIALOG_CANCELBUTTON := 122 
STATIC DEFINE ORDERDIALOG_CHEESECB := 112 
STATIC DEFINE ORDERDIALOG_CRUSTEDIT := 115 
STATIC DEFINE ORDERDIALOG_CRUSTSPINNER := 116 
STATIC DEFINE ORDERDIALOG_CRUSTSTATIC := 114 
STATIC DEFINE ORDERDIALOG_EGGPLANTCB := 111 
STATIC DEFINE ORDERDIALOG_GROUPBOX1 := 120 
STATIC DEFINE ORDERDIALOG_LARGERB := 103 
STATIC DEFINE ORDERDIALOG_MARIOBUTTON := 123 
STATIC DEFINE ORDERDIALOG_MEDIUMRB := 102 
STATIC DEFINE ORDERDIALOG_MUSHROOMCB := 110 
STATIC DEFINE ORDERDIALOG_ONIONCB1 := 107 
STATIC DEFINE ORDERDIALOG_ORDERBUTTON := 121 
STATIC DEFINE ORDERDIALOG_PEPPERCB1 := 108 
STATIC DEFINE ORDERDIALOG_PHONESTATIC := 117 
STATIC DEFINE ORDERDIALOG_SALAMICB1 := 109 
STATIC DEFINE ORDERDIALOG_SINGLELINEEDIT1 := 118 
STATIC DEFINE ORDERDIALOG_SINGLELINEEDIT2 := 119 
STATIC DEFINE ORDERDIALOG_SIZEGROUP1 := 100 
STATIC DEFINE ORDERDIALOG_SMALLRB := 101 
STATIC DEFINE ORDERDIALOG_TOMATOCB1 := 106 
STATIC DEFINE ORDERDIALOG_TOPPINGGROUP := 105 
STATIC DEFINE ORDERDIALOG_XLARGERB := 104 
#endregion

class OrderDialog inherit DIALOGWINDOW 

	protect oDCSizeGroup1 as RADIOBUTTONGROUP
	protect oCCSmallRB as RADIOBUTTON
	protect oCCMediumRB as RADIOBUTTON
	protect oCCLargeRB as RADIOBUTTON
	protect oCCXLargeRB as RADIOBUTTON
	protect oDCToppingGroup as RADIOBUTTONGROUP
	protect oDCTomatoCB1 as CHECKBOX
	protect oDCOnionCB1 as CHECKBOX
	protect oDCPepperCB1 as CHECKBOX
	protect oDCSalamiCB1 as CHECKBOX
	protect oDCMushroomCB as CHECKBOX
	protect oDCEggplantCB as CHECKBOX
	protect oDCCheeseCB as CHECKBOX
	protect oDCBroccoliCB as CHECKBOX
	protect oDCCrustEdit as SINGLELINEEDIT
	protect oDCCrustSpinner as VERTICALSPINNER
	protect oDCSingleLineEdit1 as SINGLELINEEDIT
	protect oDCSingleLineEdit2 as SINGLELINEEDIT
	protect oDCGroupBox1 as GROUPBOX
	protect oCCOrderButton as PUSHBUTTON
	protect oCCCancelButton as PUSHBUTTON
	protect oCCMarioButton as TWOBMPBUTTON

  //USER CODE STARTS HERE (do NOT remove this line)

METHOD CancelButton() 
	self:EndDialog()
	GetAppObject():Quit()
	return nil	

CONSTRUCTOR(oParent,uExtra)  

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"OrderDialog",_GetInst()},TRUE)

oCCSmallRB := RadioButton{self,ResourceID{ORDERDIALOG_SMALLRB,_GetInst()}}
oCCSmallRB:HyperLabel := HyperLabel{#SmallRB,"Small",NULL_STRING,NULL_STRING}

oCCMediumRB := RadioButton{self,ResourceID{ORDERDIALOG_MEDIUMRB,_GetInst()}}
oCCMediumRB:HyperLabel := HyperLabel{#MediumRB,"Medium",NULL_STRING,NULL_STRING}

oCCLargeRB := RadioButton{self,ResourceID{ORDERDIALOG_LARGERB,_GetInst()}}
oCCLargeRB:HyperLabel := HyperLabel{#LargeRB,"Large",NULL_STRING,NULL_STRING}

oCCXLargeRB := RadioButton{self,ResourceID{ORDERDIALOG_XLARGERB,_GetInst()}}
oCCXLargeRB:HyperLabel := HyperLabel{#XLargeRB,"X-Large",NULL_STRING,NULL_STRING}

oDCTomatoCB1 := CheckBox{self,ResourceID{ORDERDIALOG_TOMATOCB1,_GetInst()}}
oDCTomatoCB1:HyperLabel := HyperLabel{#TomatoCB1,NULL_STRING,NULL_STRING,NULL_STRING}
oDCTomatoCB1:Image := TOMATO{}
oDCTomatoCB1:TooltipText := "Tomato"

oDCOnionCB1 := CheckBox{self,ResourceID{ORDERDIALOG_ONIONCB1,_GetInst()}}
oDCOnionCB1:HyperLabel := HyperLabel{#OnionCB1,NULL_STRING,NULL_STRING,NULL_STRING}
oDCOnionCB1:Image := ONION{}
oDCOnionCB1:TooltipText := "Onion"

oDCPepperCB1 := CheckBox{self,ResourceID{ORDERDIALOG_PEPPERCB1,_GetInst()}}
oDCPepperCB1:HyperLabel := HyperLabel{#PepperCB1,NULL_STRING,NULL_STRING,NULL_STRING}
oDCPepperCB1:Image := PEPPER{}
oDCPepperCB1:TooltipText := "Pepper"

oDCSalamiCB1 := CheckBox{self,ResourceID{ORDERDIALOG_SALAMICB1,_GetInst()}}
oDCSalamiCB1:HyperLabel := HyperLabel{#SalamiCB1,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSalamiCB1:Image := SALAMI{}
oDCSalamiCB1:TooltipText := "Salami"

oDCMushroomCB := CheckBox{self,ResourceID{ORDERDIALOG_MUSHROOMCB,_GetInst()}}
oDCMushroomCB:HyperLabel := HyperLabel{#MushroomCB,NULL_STRING,NULL_STRING,NULL_STRING}
oDCMushroomCB:Image := MUSHROOM{}
oDCMushroomCB:TooltipText := "Mushroom"

oDCEggplantCB := CheckBox{self,ResourceID{ORDERDIALOG_EGGPLANTCB,_GetInst()}}
oDCEggplantCB:HyperLabel := HyperLabel{#EggplantCB,NULL_STRING,NULL_STRING,NULL_STRING}
oDCEggplantCB:Image := EGGPLANT{}
oDCEggplantCB:TooltipText := "Eggplant"

oDCCheeseCB := CheckBox{self,ResourceID{ORDERDIALOG_CHEESECB,_GetInst()}}
oDCCheeseCB:HyperLabel := HyperLabel{#CheeseCB,NULL_STRING,NULL_STRING,NULL_STRING}
oDCCheeseCB:Image := CHEESE{}
oDCCheeseCB:TooltipText := "Cheese"

oDCBroccoliCB := CheckBox{self,ResourceID{ORDERDIALOG_BROCCOLICB,_GetInst()}}
oDCBroccoliCB:HyperLabel := HyperLabel{#BroccoliCB,NULL_STRING,NULL_STRING,NULL_STRING}
oDCBroccoliCB:Image := BROCOLI{}
oDCBroccoliCB:TooltipText := "Broccoli"

oDCCrustEdit := SingleLineEdit{self,ResourceID{ORDERDIALOG_CRUSTEDIT,_GetInst()}}
oDCCrustEdit:HyperLabel := HyperLabel{#CrustEdit,NULL_STRING,NULL_STRING,NULL_STRING}

oDCCrustSpinner := VerticalSpinner{self,ResourceID{ORDERDIALOG_CRUSTSPINNER,_GetInst()}}
oDCCrustSpinner:Range := Range{0,10}
oDCCrustSpinner:HyperLabel := HyperLabel{#CrustSpinner,NULL_STRING,NULL_STRING,NULL_STRING}

oDCSingleLineEdit1 := SingleLineEdit{self,ResourceID{ORDERDIALOG_SINGLELINEEDIT1,_GetInst()}}
oDCSingleLineEdit1:HyperLabel := HyperLabel{#SingleLineEdit1,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSingleLineEdit1:TextLimit := 3

oDCSingleLineEdit2 := SingleLineEdit{self,ResourceID{ORDERDIALOG_SINGLELINEEDIT2,_GetInst()}}
oDCSingleLineEdit2:HyperLabel := HyperLabel{#SingleLineEdit2,NULL_STRING,NULL_STRING,NULL_STRING}
oDCSingleLineEdit2:TextLimit := 4

oDCGroupBox1 := GroupBox{self,ResourceID{ORDERDIALOG_GROUPBOX1,_GetInst()}}
oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1,"Your Selection",NULL_STRING,NULL_STRING}

oCCOrderButton := PushButton{self,ResourceID{ORDERDIALOG_ORDERBUTTON,_GetInst()}}
oCCOrderButton:HyperLabel := HyperLabel{#OrderButton,NULL_STRING,NULL_STRING,NULL_STRING}
oCCOrderButton:Image := PIZZA{}
oCCOrderButton:TooltipText := "Press to get a fresh made pizza delivered to your house."

oCCCancelButton := PushButton{self,ResourceID{ORDERDIALOG_CANCELBUTTON,_GetInst()}}
oCCCancelButton:HyperLabel := HyperLabel{#CancelButton,"Push",NULL_STRING,NULL_STRING}
oCCCancelButton:Image := NOPIZZA{}
oCCCancelButton:TooltipText := "Press in case you already had enough pizzas."

oCCMarioButton := TWOBMPBUTTON{self,ResourceID{ORDERDIALOG_MARIOBUTTON,_GetInst()}}
oCCMarioButton:HyperLabel := HyperLabel{#MarioButton,NULL_STRING,NULL_STRING,NULL_STRING}
oCCMarioButton:TooltipText := "Press to see Mario smile ..."

oDCSizeGroup1 := RadioButtonGroup{self,ResourceID{ORDERDIALOG_SIZEGROUP1,_GetInst()}}
oDCSizeGroup1:FillUsing({ ;
							{oCCSmallRB,"SmallRB"}, ;
							{oCCMediumRB,"MediumRB"}, ;
							{oCCLargeRB,"LargeRB"}, ;
							{oCCXLargeRB,"XLargeRB"} ;
							})
oDCSizeGroup1:HyperLabel := HyperLabel{#SizeGroup1,"Select Pizza Size",NULL_STRING,NULL_STRING}

oDCToppingGroup := RadioButtonGroup{self,ResourceID{ORDERDIALOG_TOPPINGGROUP,_GetInst()}}
oDCToppingGroup:HyperLabel := HyperLabel{#ToppingGroup,"Select Toppings",NULL_STRING,NULL_STRING}

self:Caption := "Marios Pizza Blitz"
self:HyperLabel := HyperLabel{#OrderDialog,"Marios Pizza Blitz",NULL_STRING,NULL_STRING}
self:Icon := MARIOICON{}

self:PostInit(oParent,uExtra)

return self


METHOD OrderButton( ) 
	local oSubmitDlg as SubmitDlg

	oSubmitDlg := SubmitDlg{self}
	oSubmitDlg:Show()
	return nil

method PostInit() 
	oCCMarioButton:BmpUnPressed := Bitmap{ResourceID{"CHEF1", _GetInst()}}
	oCCMarioButton:BmpPressed   := Bitmap{ResourceID{"CHEF2", _GetInst()}}
	return nil	

method VerticalSpin( oSpinEvent ) 
	super:VerticalSpin(oSpinEvent)

  if oSpinEvent:Spinner == oDCCrustSpinner
  	oDCCrustEdit:TextValue := LTrim(AsString(oSpinEvent:Value))
  endif	

	return NIL



END CLASS
