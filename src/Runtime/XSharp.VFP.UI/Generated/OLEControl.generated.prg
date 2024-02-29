
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



	END CLASS
	PARTIAL CLASS OLEBoundControl  INHERIT OleControl

	END CLASS
	END NAMESPACE      