//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// PrinterEvents.prg
/*
CLASS PrinterErrorEvent INHERIT @@Event
//RvdH 061218 Declared properties for performance
ACCESS ErrorType AS DWORD STRICT 

RETURN wParam

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 

SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 
END CLASS

CLASS PrinterExposeEvent INHERIT @@Event
//RvdH 061218 Declared properties for performance
ACCESS ExposedArea AS OBJECT STRICT 


RETURN oWindow

CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 

SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


RETURN 

ACCESS PageNo AS DWORD STRICT 


RETURN wParam

END CLASS
*/
