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
#endregion

PARTIAL CLASS OrderDialog INHERIT DIALOGWINDOW
	PROTECT oDCSizeGroup1 AS RADIOBUTTONGROUP
	PROTECT oCCSmallRB AS RADIOBUTTON
	PROTECT oCCMediumRB AS RADIOBUTTON
	PROTECT oCCLargeRB AS RADIOBUTTON
	PROTECT oCCXLargeRB AS RADIOBUTTON
	PROTECT oDCToppingGroup AS RADIOBUTTONGROUP
	PROTECT oDCTomatoCB1 AS CHECKBOX
	PROTECT oDCOnionCB1 AS CHECKBOX
	PROTECT oDCPepperCB1 AS CHECKBOX
	PROTECT oDCSalamiCB1 AS CHECKBOX
	PROTECT oDCMushroomCB AS CHECKBOX
	PROTECT oDCEggplantCB AS CHECKBOX
	PROTECT oDCCheeseCB AS CHECKBOX
	PROTECT oDCBroccoliCB AS CHECKBOX
	PROTECT oDCCrustStatic AS FIXEDTEXT
	PROTECT oDCCrustEdit AS SINGLELINEEDIT
	PROTECT oDCCrustSpinner AS VERTICALSPINNER
	PROTECT oDCPhoneStatic AS FIXEDTEXT
	PROTECT oDCSingleLineEdit1 AS SINGLELINEEDIT
	PROTECT oDCSingleLineEdit2 AS SINGLELINEEDIT
	PROTECT oDCGroupBox1 AS GROUPBOX
	PROTECT oCCOrderButton AS PUSHBUTTON
	PROTECT oCCCancelButton AS PUSHBUTTON
	PROTECT oCCMarioButton AS TWOBMPBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)


METHOD CancelButton()
	SELF:EndDialog()
	GetAppObject():Quit()
	RETURN NIL

CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

	SUPER(oParent , ResourceID{"OrderDialog" , _GetInst()} , TRUE)

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
	SELF:oDCTomatoCB1:HyperLabel := HyperLabel{#TomatoCB1 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCTomatoCB1:TooltipText := "Tomato"
	SELF:oDCTomatoCB1:Image := TOMATO{}

	SELF:oDCOnionCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_ONIONCB1  , _GetInst() } }
	SELF:oDCOnionCB1:HyperLabel := HyperLabel{#OnionCB1 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCOnionCB1:TooltipText := "Onion"
	SELF:oDCOnionCB1:Image := ONION{}

	SELF:oDCPepperCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_PEPPERCB1  , _GetInst() } }
	SELF:oDCPepperCB1:HyperLabel := HyperLabel{#PepperCB1 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCPepperCB1:TooltipText := "Pepper"
	SELF:oDCPepperCB1:Image := PEPPER{}

	SELF:oDCSalamiCB1 := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_SALAMICB1  , _GetInst() } }
	SELF:oDCSalamiCB1:HyperLabel := HyperLabel{#SalamiCB1 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCSalamiCB1:TooltipText := "Salami"
	SELF:oDCSalamiCB1:Image := SALAMI{}

	SELF:oDCMushroomCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_MUSHROOMCB  , _GetInst() } }
	SELF:oDCMushroomCB:HyperLabel := HyperLabel{#MushroomCB , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCMushroomCB:TooltipText := "Mushroom"
	SELF:oDCMushroomCB:Image := MUSHROOM{}

	SELF:oDCEggplantCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_EGGPLANTCB  , _GetInst() } }
	SELF:oDCEggplantCB:HyperLabel := HyperLabel{#EggplantCB , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCEggplantCB:TooltipText := "Eggplant"
	SELF:oDCEggplantCB:Image := EGGPLANT{}

	SELF:oDCCheeseCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_CHEESECB  , _GetInst() } }
	SELF:oDCCheeseCB:HyperLabel := HyperLabel{#CheeseCB , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCCheeseCB:TooltipText := "Cheese"
	SELF:oDCCheeseCB:Image := CHEESE{}

	SELF:oDCBroccoliCB := CHECKBOX{SELF , ResourceID{ ORDERDIALOG_BROCCOLICB  , _GetInst() } }
	SELF:oDCBroccoliCB:HyperLabel := HyperLabel{#BroccoliCB , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCBroccoliCB:TooltipText := "Broccoli"
	SELF:oDCBroccoliCB:Image := BROCOLI{}

	SELF:oDCCrustStatic := FIXEDTEXT{SELF , ResourceID{ ORDERDIALOG_CRUSTSTATIC  , _GetInst() } }
	SELF:oDCCrustStatic:HyperLabel := HyperLabel{#CrustStatic , "Crust &Size" , NULL_STRING , NULL_STRING}

	SELF:oDCCrustEdit := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_CRUSTEDIT  , _GetInst() } }
	SELF:oDCCrustEdit:HyperLabel := HyperLabel{#CrustEdit , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oDCCrustSpinner := VERTICALSPINNER{SELF , ResourceID{ ORDERDIALOG_CRUSTSPINNER  , _GetInst() } }
	SELF:oDCCrustSpinner:HyperLabel := HyperLabel{#CrustSpinner , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCCrustSpinner:Range := Range{ , 10}

	SELF:oDCPhoneStatic := FIXEDTEXT{SELF , ResourceID{ ORDERDIALOG_PHONESTATIC  , _GetInst() } }
	SELF:oDCPhoneStatic:HyperLabel := HyperLabel{#PhoneStatic , "&Your Phone" , NULL_STRING , NULL_STRING}

	SELF:oDCSingleLineEdit1 := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_SINGLELINEEDIT1  , _GetInst() } }
	SELF:oDCSingleLineEdit1:HyperLabel := HyperLabel{#SingleLineEdit1 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCSingleLineEdit1:TextLimit := 3

	SELF:oDCSingleLineEdit2 := SINGLELINEEDIT{SELF , ResourceID{ ORDERDIALOG_SINGLELINEEDIT2  , _GetInst() } }
	SELF:oDCSingleLineEdit2:HyperLabel := HyperLabel{#SingleLineEdit2 , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oDCSingleLineEdit2:TextLimit := 4

	SELF:oDCGroupBox1 := GROUPBOX{SELF , ResourceID{ ORDERDIALOG_GROUPBOX1  , _GetInst() } }
	SELF:oDCGroupBox1:HyperLabel := HyperLabel{#GroupBox1 , "Your Selection" , NULL_STRING , NULL_STRING}

	SELF:oCCOrderButton := PUSHBUTTON{SELF , ResourceID{ ORDERDIALOG_ORDERBUTTON  , _GetInst() } }
	SELF:oCCOrderButton:HyperLabel := HyperLabel{#OrderButton , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oCCOrderButton:TooltipText := "Press to get a fresh made pizza delivered to your house."
	SELF:oCCOrderButton:Image := PIZZA{}

	SELF:oCCCancelButton := PUSHBUTTON{SELF , ResourceID{ ORDERDIALOG_CANCELBUTTON  , _GetInst() } }
	SELF:oCCCancelButton:HyperLabel := HyperLabel{#CancelButton , "Push" , NULL_STRING , NULL_STRING}
	SELF:oCCCancelButton:TooltipText := "Press in case you already had enough pizzas. "
	SELF:oCCCancelButton:Image := NOPIZZA{}

	SELF:oCCMarioButton := TWOBMPBUTTON{SELF , ResourceID{ ORDERDIALOG_MARIOBUTTON  , _GetInst() } }
	SELF:oCCMarioButton:HyperLabel := HyperLabel{#MarioButton , NULL_STRING , NULL_STRING , NULL_STRING}
	SELF:oCCMarioButton:TooltipText := "Press to see Mario smile ..."

	SELF:oDCSizeGroup1:FillUsing({ ;
										{SELF:oCCSmallRB, "SmallRB"}, ;
										{SELF:oCCMediumRB, "MediumRB"}, ;
										{SELF:oCCLargeRB, "LargeRB"}, ;
										{SELF:oCCXLargeRB, "XLargeRB"} ;
										})

	SELF:Caption := "Marios Pizza Blitz"
	SELF:HyperLabel := HyperLabel{#OrderDialog , "Marios Pizza Blitz" , NULL_STRING , NULL_STRING}
	SELF:Icon := MARIOICON{}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD OrderButton( )
	LOCAL oSubmitDlg AS SubmitDlg

	oSubmitDlg := SubmitDlg{SELF}
	oSubmitDlg:Show()
	RETURN NIL

METHOD PostInit()
	oCCMarioButton:BmpUnPressed := Bitmap{ResourceID{"CHEF1", _GetInst()}}
	oCCMarioButton:BmpPressed   := Bitmap{ResourceID{"CHEF2", _GetInst()}}
	RETURN NIL

METHOD VerticalSpin( oSpinEvent AS SpinnerEvent )
	SUPER:VerticalSpin(oSpinEvent)

  IF oSpinEvent:Spinner == oDCCrustSpinner
  	oDCCrustEdit:TextValue := LTrim(AsString(oSpinEvent:@@Value))
  ENDIF

	RETURN NIL



END CLASS
