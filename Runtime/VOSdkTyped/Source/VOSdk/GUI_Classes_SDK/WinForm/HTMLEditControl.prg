
#USING System.Windows.Forms

CLASS VOHtmlEditorControl INHERIT WebBrowser IMPLEMENTS IVOControl
	//PRIVATE lBusy		AS LOGIC

	#include "PropControl.vh"
	PROPERTY oHTML		AS XSharp.VO.HTMLControl GET (XSharp.VO.HTMLControl) SELF:Control


	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		
	METHOD SetVisualStyle AS VOID STRICT
		SELF:AutoSize			:= FALSE
		IF SELF:oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF
	

	VIRTUAL PROPERTY Text AS STRING GET SUPER:Text SET SUPER:Text := Value

	


END CLASS
