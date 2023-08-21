// 493. VO compatibility isssues with VOSTRUCts
// this is a direct port from a test VO app that compiles without errors
// VO's behavior is strange, some code snippets give correct results at runtime
// although they probably shouldn't, see inline. But I guess we need to be compatible
// for projects that get directly ported from VO.
// Also there's a runtime crash when calling ListBox:EnableItemDrag()
// but that's probably due to a similar issue in the already compiled in vulcan GUI classes
#ifdef __XSHARP__
	STATIC DEFINE DATAWIN_LISTBOX1 := 100 
	STATIC DEFINE DATAWIN_TEST := 101 
	STATIC DEFINE DATAWIN_LISTBOX2 := 102 
	STATIC DEFINE DATAWIN_TEST2 := 103 
#else 
	#DEFINE DATAWIN_LISTBOX1  100 
	#DEFINE DATAWIN_TEST  101 
	#DEFINE DATAWIN_LISTBOX2  102 
	#DEFINE DATAWIN_TEST2  103 

#endif
#undef VO

FUNCTION Start() AS INT
LOCAL oApp AS xApp
oApp := xApp{}
oApp:Start()
RETURN 0

CLASS xApp INHERIT App
METHOD Start() 
	LOCAL oMainWindow AS ShellWindow
	LOCAL oDataWin AS datawin
	oMainWindow := ShellWindow{SELF}
	oMainWindow:Show(1)
	
	oDataWin := datawin{oMainWindow}
	oDataWin:Show(1)
	
	SELF:Exec()
RETURN NIL

END CLASS
CLASS datawin INHERIT DATAWINDOW 

	PROTECT oDCListBox1 AS LISTBOX
	PROTECT oCCTest AS PUSHBUTTON
	PROTECT oDCListBox2 AS LISTBOX
	PROTECT oCCTest2 AS PUSHBUTTON

  //{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)

CONSTRUCTOR(oWindow,iCtlID,oServer,uExtra)  

SELF:PreInit(oWindow,iCtlID,oServer,uExtra)

SUPER(oWindow,ResourceID{"datawin",_GetInst()},iCtlID)

oDCListBox1 := ListBox{SELF,ResourceID{DATAWIN_LISTBOX1,_GetInst()}}
oDCListBox1:HyperLabel := HyperLabel{#ListBox1,NULL_STRING,NULL_STRING,NULL_STRING}

oCCTest := PushButton{SELF,ResourceID{DATAWIN_TEST,_GetInst()}}
oCCTest:HyperLabel := HyperLabel{#Test,"Test with local",NULL_STRING,NULL_STRING}

oDCListBox2 := ListBox{SELF,ResourceID{DATAWIN_LISTBOX2,_GetInst()}}
oDCListBox2:HyperLabel := HyperLabel{#ListBox2,NULL_STRING,NULL_STRING,NULL_STRING}

oCCTest2 := PushButton{SELF,ResourceID{DATAWIN_TEST2,_GetInst()}}
oCCTest2:HyperLabel := HyperLabel{#Test2,"Test with struct",NULL_STRING,NULL_STRING}

SELF:Caption := "DataWindow Caption"
SELF:HyperLabel := HyperLabel{#datawin,"DataWindow Caption",NULL_STRING,NULL_STRING}

IF !IsNil(oServer)
	SELF:Use(oServer)
ENDIF

SELF:PostInit(oWindow,iCtlID,oServer,uExtra)

RETURN


METHOD PostInit(oWindow,iCtlID,oServer,uExtra) 
	//Put your PostInit additions here
	LOCAL n AS INT
	FOR n := 1 UPTO 100
		SELF:oDCListBox1:AddItem(Replicate(AsString(n) , 20))
	NEXT
	
	#warning calls to EnableItemDrag() cause a runtime crash
//	SELF:oDCListBox1:EnableItemDrag()
//	SELF:oDCListBox2:EnableItemDrag()
	
	RETURN NIL


METHOD Test( ) 
LOCAL h AS PTR
LOCAL ptIS IS _winPoint
LOCAL ptAS AS _winPoint
LOCAL nRes AS INT
h := SELF:oDCListBox1:Handle()
ptAS := MemAlloc(100) 
ptAS:x := 200
ptAS:y := 200 
ptIS:x := 200
ptIS:y := 200 

// Next 2 statements compile in VO and return correct results:
// IS local, AS param
#ifdef VO
	nRes := LBItemFromPt_AS(h , ptIS , FALSE)
	SELF:oDCListBox2:AddItem("LOCAL IS, FUNC AS: " + AsString(nRes))
	? nRes
	xAssert(nRes > 0)
	
//	 IS local, IS param
	nRes := LBItemFromPt_IS(h , ptIS , FALSE)
	SELF:oDCListBox2:AddItem("LOCAL IS, FUNC IS: " + AsString(nRes))
	? nRes
	xAssert(nRes > 0)
#endif

// Next 2 statements compile in VO but do not return the listbox item
// AS local, AS param
nRes := LBItemFromPt_AS(h , ptAS , FALSE)
SELF:oDCListBox2:AddItem("LOCAL AS, FUNC AS: " + AsString(nRes))
? nRes

// AS local, IS param 
#ifdef VO
	nRes := LBItemFromPt_IS(h , ptAS , FALSE)
	SELF:oDCListBox2:AddItem("LOCAL AS, FUNC IS: " + AsString(nRes))
	? nRes
#endif

// correct results as expected
? nRes := LB_AS(ptAS)
xAssert(nRes == 200)

? nRes := LB_IS(ptIS)
xAssert(nRes == 200)
? nRes := LB_AS(@ptIS)
xAssert(nRes == 200)

MemFree(ptAS)

RETURN NIL

METHOD Test2( )  

LOCAL h AS PTR
LOCAL ptAS AS _winTest
LOCAL ptIS IS _winTest
LOCAL nRes AS INT

ptAS := MemAlloc(20)

h := SELF:oDCListBox1:Handle() 
ptAS:ptCursor:x := 200
ptAS:ptCursor:y := 200 
ptIS:ptCursor:x := 200
ptIS:ptCursor:y := 200 

// this compiles in VO and finds the item (AS local, AS param):
#ifdef VO
	nRes := LBItemFromPt_AS(h , ptAS:ptCursor , FALSE)
	SELF:oDCListBox2:AddItem("ptAS, AS: " + AsString(nRes))
	? nRes
	xAssert(nRes > 0)
#endif

// This compiles in VO, doesn't find the item (AS local, IS param)
nRes := LBItemFromPt_IS(h , ptAS:ptCursor , FALSE)
SELF:oDCListBox2:AddItem("ptAS, IS: " + AsString(nRes))



// this compiles in VO and finds the item (IS local, AS param):
#ifdef VO
	nRes := LBItemFromPt_AS(h , ptIS:ptCursor , FALSE)
	SELF:oDCListBox2:AddItem("ptIS, AS: " + AsString(nRes))
	xAssert(nRes > 0)
#endif

// This compiles in VO, doesn't find the item (IS local, IS param):
nRes := LBItemFromPt_IS(h , ptIS:ptCursor , FALSE)
SELF:oDCListBox2:AddItem("ptIS, IS: " + AsString(nRes))

MemFree(ptAS)

RETURN NIL

END CLASS
_DLL FUNCTION LBItemFromPt_AS(hLB AS PTR, pt AS _winPoint, bAutoScroll AS LOGIC) AS INT STRICT:comctl32.LBItemFromPt

_DLL FUNCTION LBItemFromPt_IS(hLB AS PTR, pt IS _winPoint, bAutoScroll AS LOGIC) AS INT STRICT:comctl32.LBItemFromPt

FUNCTION LB_AS(pt AS _winPOINT) AS INT
RETURN pt:x

FUNCTION LB_IS(pt IS _winPOINT) AS INT
RETURN pt:x



VOSTRUCT _winTest
MEMBER ptCursor IS _winPoint

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

