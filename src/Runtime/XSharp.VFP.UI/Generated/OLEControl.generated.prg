//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

// Class OLEControl  BaseClass   Listbox Class  Listbox
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS OLEControl  IMPLEMENTS IVFPList, IVFPOwner, IVFPGraphics
#include "VFPList.xh"
#include "VFPContainer.xh"
#include "VFPItems.xh"

		PROPERTY AutoHideScrollBar AS LONG AUTO
		PROPERTY ColumnCount AS LONG AUTO
		PROPERTY ColumnLines AS LOGIC AUTO
		PROPERTY ColumnWidths AS STRING AUTO

		PROPERTY NullDisplay AS String AUTO
		PROPERTY Picture AS STRING AUTO
		METHOD SetFocus() AS VOID STRICT
			//SELF:Focus()

		// IVFPList stub — OLEControl has no list implementation
		METHOD AddItem(cItem, nIndex, nColumn) AS VOID CLIPPER
			RETURN
		METHOD Clear() AS VOID CLIPPER
			RETURN
		PROPERTY DisplayValue AS USUAL AUTO
		PROPERTY ListCount AS LONG GET 0
		PROPERTY ListIndex AS LONG GET 0 SET
		METHOD RemoveItem(nIndex AS LONG) AS VOID
			RETURN
		METHOD Requery() AS VOID STRICT
			RETURN
		PROPERTY RowSource AS STRING AUTO
		PROPERTY RowSourceType AS LONG AUTO



	END CLASS
	PARTIAL CLASS OLEBoundControl  INHERIT OleControl

	END CLASS
	END NAMESPACE      
