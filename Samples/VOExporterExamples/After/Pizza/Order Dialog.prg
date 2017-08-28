#region DEFINES
STATIC DEFINE ORDERDIALOG_SIZEGROUP1 := 100
STATIC DEFINE ORDERDIALOG_SMALLRB := 101 
STATIC DEFINE ORDERDIALOG_MEDIUMRB := 102 
STATIC DEFINE ORDERDIALOG_LARGERB := 103 
STATIC DEFINE ORDERDIALOG_XLARGERB := 104 
STATIC DEFINE ORDERDIALOG_TOPPINGGROUP := 105 
STATIC DEFINE ORDERDIALOG_TOMATOCB1 := 106 
STATIC DEFINE ORDERDIALOG_ONIONCB1 := 107 
STATIC DEFINE ORDERDIALOG_PEPPERCB1 := 108 
STATIC DEFINE ORDERDIALOG_SALAMICB1 := 109 
STATIC DEFINE ORDERDIALOG_MUSHROOMCB := 110 
STATIC DEFINE ORDERDIALOG_EGGPLANTCB := 111
STATIC DEFINE ORDERDIALOG_CHEESECB := 112
STATIC DEFINE ORDERDIALOG_BROCCOLICB := 113
STATIC DEFINE ORDERDIALOG_CRUSTSTATIC := 114
STATIC DEFINE ORDERDIALOG_CRUSTEDIT := 115 
STATIC DEFINE ORDERDIALOG_CRUSTSPINNER := 116 
STATIC DEFINE ORDERDIALOG_PHONESTATIC := 117
STATIC DEFINE ORDERDIALOG_SINGLELINEEDIT1 := 118
STATIC DEFINE ORDERDIALOG_SINGLELINEEDIT2 := 119
STATIC DEFINE ORDERDIALOG_GROUPBOX1 := 120
STATIC DEFINE ORDERDIALOG_ORDERBUTTON := 121
STATIC DEFINE ORDERDIALOG_CANCELBUTTON := 122
STATIC DEFINE ORDERDIALOG_MARIOBUTTON := 123 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT1 := 100 
STATIC DEFINE HELPABOUT_THEFIXEDICON1 := 101 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT2 := 102 
STATIC DEFINE HELPABOUT_THEFIXEDTEXT3 := 103 
STATIC DEFINE HELPABOUT_PUSHBUTTON1 := 104 
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

	// {{%UC%}} User code starts here (DO NOT remove this line)  

CONSTRUCTOR(oParent,uExtra)  

self:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"OrderDialog",_GetInst()},TRUE)

	SELF:oDCSizeGroup1 := RADIOBUTTONGROUP{SELF , ResourceID{ ORDERDIALOG_SIZEGROUP1  , _GetInst() } }
	SELF:oDCSizeGroup1:HyperLabel := HyperLabel{#SizeGroup1 , "Select Pizza Size" , NULL_STRING , NULL_STRING}
	SELF:oCCSmallRB := RADIOBUTTON{SELF , ResourceID{ ORDERDIALOG_SMALLRB  , _GetInst() } }
	SELF:oCCSmallRB:HyperLabel := HyperLabel{#SmallRB , "Small" , NULL_STRING , NULL_STRING}

	SELF:oCCMediumRB := RADIOBUTTON{SELF , ResourceID{ ORDERDIALOG_MEDIUMRB  , _GetInst() } }
	SELF:oCCMediumRB:HyperLabel := HyperLabel{#MediumRB , "Medium" , NULL_STRING , NULL_STRING}

	SELF:oCCLargeRB := RADIOBUTTON{SELF , ResourceID{ ORDERDIALOG_LARGERB  , _GetInst() } }
	SELF:oCCLargeRB:HyperLabel := HyperLabel{#LargeRB , "Large" , NULL_STRING , NULL_STRING}

	SELF:oCCXLargeRB := RADIOBUTTON{SELF , ResourceID{ ORDERDIALOG_XLARGERB  , _GetInst() } }
	SELF:oCCXLargeRB:HyperLabel := HyperLabel{#XLargeRB , "X-Large" , NULL_STRING , NULL_STRING}

	SELF:oDCToppingGroup := RADIOBUTTONGROUP{SELF , ResourceID{ ORDERDIALOG_TOPPINGGROUP  , _GetInst() } }
	SELF:oDCToppingGroup:HyperLabel := HyperLabel{#ToppingGroup , "Select Toppings" , NULL_STRING , NULL_STRING}

	SELF:oDCTomatoCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_TOMATOCB1  , _GetInst() } }
	SELF:oDCTomatoCB1:TooltipText := "Tomato"
	SELF:oDCTomatoCB1:Image := TOMATO{}
	SELF:oDCTomatoCB1:HyperLabel := HyperLabel{#TomatoCB1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCOnionCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_ONIONCB1  , _GetInst() } }
	SELF:oDCOnionCB1:TooltipText := "Onion"
	SELF:oDCOnionCB1:Image := ONION{}
	SELF:oDCOnionCB1:HyperLabel := HyperLabel{#OnionCB1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCPepperCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_PEPPERCB1  , _GetInst() } }
	SELF:oDCPepperCB1:TooltipText := "Pepper"
	SELF:oDCPepperCB1:Image := PEPPER{}
	SELF:oDCPepperCB1:HyperLabel := HyperLabel{#PepperCB1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSalamiCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_SALAMICB1  , _GetInst() } }
	SELF:oDCSalamiCB1:TooltipText := "Salami"
	SELF:oDCSalamiCB1:Image := SALAMI{}
	SELF:oDCSalamiCB1:HyperLabel := HyperLabel{#SalamiCB1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCMushroomCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_MUSHROOMCB  , _GetInst() } }
	SELF:oDCMushroomCB:TooltipText := "Mushroom"
	SELF:oDCMushroomCB:Image := MUSHROOM{}
	SELF:oDCMushroomCB:HyperLabel := HyperLabel{#MushroomCB , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCEggplantCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_EGGPLANTCB  , _GetInst() } }
	SELF:oDCEggplantCB:TooltipText := "Eggplant"
	SELF:oDCEggplantCB:Image := EGGPLANT{}
	SELF:oDCEggplantCB:HyperLabel := HyperLabel{#EggplantCB , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCheeseCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_CHEESECB  , _GetInst() } }
	SELF:oDCCheeseCB:TooltipText := "Cheese"
	SELF:oDCCheeseCB:Image := CHEESE{}
	SELF:oDCCheeseCB:HyperLabel := HyperLabel{#CheeseCB , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCBroccoliCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_BROCCOLICB  , _GetInst() } }
	SELF:oDCBroccoliCB:TooltipText := "Broccoli"
	SELF:oDCBroccoliCB:Image := BROCOLI{}
	SELF:oDCBroccoliCB:HyperLabel := HyperLabel{#BroccoliCB , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCrustEdit := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_CRUSTEDIT  , _GetInst() } }
	SELF:oDCCrustEdit:HyperLabel := HyperLabel{#CrustEdit , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCrustSpinner := VERTICALSPINNER{SELF , ResourceID{ ORDERDIALOG_CRUSTSPINNER  , _GetInst() } }
	SELF:oDCCrustSpinner:HyperLabel := HyperLabel{#CrustSpinner , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCCrustSpinner:Range := Range{ , 10}

	SELF:oDCSingleLineEdit1 := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_SINGLELINEEDIT1  , _GetInst() } }
	SELF:oDCSingleLineEdit1:TextLimit := 3
	SELF:oDCSingleLineEdit1:HyperLabel := HyperLabel{#SingleLineEdit1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSingleLineEdit2 := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_SINGLELINEEDIT2  , _GetInst() } }
	SELF:oDCSingleLineEdit2:TextLimit := 4
	SELF:oDCSingleLineEdit2:HyperLabel := HyperLabel{#SingleLineEdit2 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCGroupBox1 := GROUPBOX{SELF , ResourceID{ ORDERDIALOG_GROUPBOX1  , _GetInst() } }
	SELF:oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1 , "Your Selection" , NULL_STRING , NULL_STRING}

	SELF:oCCOrderButton := PUSHBUTTON{SELF , ResourceID{ ORDERDIALOG_ORDERBUTTON  , _GetInst() } }
	SELF:oCCOrderButton:TooltipText := "Press to get a fresh made pizza delivered to your house."
	SELF:oCCOrderButton:Image := ImgPIZZA{}
	SELF:oCCOrderButton:HyperLabel := HyperLabel{#OrderButton , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCCancelButton := PUSHBUTTON{SELF , ResourceID{ ORDERDIALOG_CANCELBUTTON  , _GetInst() } }
	SELF:oCCCancelButton:TooltipText := "Press in case you already had enough pizzas."
	SELF:oCCCancelButton:Image := NOPIZZA{}
	SELF:oCCCancelButton:HyperLabel := HyperLabel{#CancelButton , "Push" , NULL_STRING , NULL_STRING}

	SELF:oCCMarioButton := TWOBMPBUTTON{SELF , ResourceID{ ORDERDIALOG_MARIOBUTTON  , _GetInst() } }
	SELF:oCCMarioButton:TooltipText := "Press to see Mario smile ..."
	SELF:oCCMarioButton:HyperLabel := HyperLabel{#MarioButton , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCSizeGroup1:FillUsing({ ;
										{SELF:oCCSmallRB, "SmallRB"}, ;
										{SELF:oCCMediumRB, "MediumRB"}, ;
										{SELF:oCCLargeRB, "LargeRB"}, ;
										{SELF:oCCXLargeRB, "XLargeRB"} ;
							})


self:Caption := "Marios Pizza Blitz"
	SELF:Icon := MARIOICON{}
self:HyperLabel := HyperLabel{#OrderDialog,"Marios Pizza Blitz",NULL_STRING,NULL_STRING}

self:PostInit(oParent,uExtra)

RETURN


METHOD OrderButton( ) 
	local oSubmitDlg as SubmitDlg

	oSubmitDlg := SubmitDlg{self}
	oSubmitDlg:Show()
	return nil

METHOD CancelButton() 
	self:EndDialog()
	GetAppObject():Quit()
	return nil	

method VerticalSpin( oSpinEvent ) 
	super:VerticalSpin(oSpinEvent)

  if oSpinEvent:Spinner == oDCCrustSpinner
  	oDCCrustEdit:TextValue := LTrim(AsString(oSpinEvent:Value))
  endif	

	return NIL



method PostInit() 
	oCCMarioButton:BmpUnPressed := Bitmap{ResourceID{"CHEF1", _GetInst()}}
	oCCMarioButton:BmpPressed   := Bitmap{ResourceID{"CHEF2", _GetInst()}}
	return nil	

END CLASS
