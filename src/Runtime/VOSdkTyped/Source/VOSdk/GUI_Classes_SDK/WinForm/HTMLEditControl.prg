//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using SWF := System.Windows.Forms
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
class VOHtmlEditorControl inherit SWF.WebBrowser implements IVOControlProperties
	//PRIVATE lBusy		AS LOGIC

	#include "PropControl.xh"
	PROPERTY oHTML		AS VOSDK.HTMLControl GET (VOSDK.HTMLControl) SELF:Control

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()

	METHOD SetVisualStyle AS VOID STRICT
		SELF:AutoSize			:= FALSE
		IF SELF:oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF


	VIRTUAL PROPERTY Text AS STRING GET SUPER:Text SET SUPER:Text := Value




END CLASS
