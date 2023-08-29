//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// TabControl.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control
USING SWF := System.Windows.Forms
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK

class VOSplitContainer inherit SWF.SplitContainer implements IVOControlProperties, IVOControlInitialize

    PROPERTY oSplitView		AS VOSDK.SplitView GET (VOSDK.SplitView) SELF:Control
	#include "PropControlStyle.xh"

	METHOD Initialize() AS VOID STRICT
		SELF:AutoSize			:= FALSE
		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER()
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SELF:Initialize()
		SELF:SetVisualStyle()



END CLASS
