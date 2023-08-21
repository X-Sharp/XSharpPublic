// 446. error XS1729: 'ControlEvent' does not contain a constructor that takes 2 arguments
// VO inciompatibility. I think when no constructor is provided in the code then the auto generated
// constructor must be given CLIPPER calling convention, if the base constructor is CLIPPER
FUNCTION Start() AS VOID
	LOCAL oEvent AS @@Event
	oEvent := @@Event{NULL_PTR , 1}
	? oEvent:uMsg
	xAssert(oEvent:uMsg == 1)

	oEvent := ControlEvent{NULL_PTR , 2}
	? oEvent:uMsg
	xAssert(oEvent:uMsg == 2)

	oEvent := AnotherEvent{NULL_PTR , 3 , 5}
	? oEvent:uMsg
	xAssert(oEvent:uMsg == 3)

	oEvent := DeeperEvent{NULL_PTR , 4 , 5, 7}
	? oEvent:uMsg
	xAssert(oEvent:uMsg == 4)
RETURN

CLASS @@Event
	EXPORT hWnd 	AS PTR
	EXPORT uMsg 	AS DWORD
	EXPORT wParam 	AS DWORD
	EXPORT lParam 	AS LONG
	EXPORT oWindow AS OBJECT 
CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	? _hWnd, _uMsg, _wParam, _lParam, _oWindow
	SELF:hWnd := _hWnd
	SELF:uMsg := _uMsg
	SELF:wParam := _wParam
	SELF:lParam := _lParam
	SELF:oWindow := _oWindow
END CLASS

CLASS ControlEvent INHERIT Event
END CLASS

CLASS AnotherEvent INHERIT Event
END CLASS

CLASS DeeperEvent INHERIT ControlEvent
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
CLASS MyException INHERIT Exception
END CLASS	
