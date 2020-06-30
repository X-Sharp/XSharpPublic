// TabControl.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK

CLASS VOSplitContainer INHERIT System.Windows.Forms.SplitContainer IMPLEMENTS IVOControl, IVOControlInitialize

    PROPERTY oSplitView		AS VOSDK.SplitView GET (VOSDK.SplitView) SELF:Control

	#include "PropControl.vh"

	METHOD Initialize() AS VOID STRICT
		SELF:AutoSize			:= FALSE
		RETURN
	
	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER()
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SELF:Initialize()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT


END CLASS
