
// Class PageFrame   BaseClass   Pageframe  Class  Pageframe
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS PageFrame   IMPLEMENTS IVFPControl, IVFPOwner
#include "VFPControl.xh"
#include "VFPContainer.xh"
#include "VFPGroup.xh"

		PROPERTY ActivePage AS LONG AUTO
		PROPERTY BorderColor AS LONG AUTO
		PROPERTY BorderWidth  AS LONG AUTO
		PROPERTY PageCount  AS LONG AUTO
		PROPERTY PageHeight  AS LONG AUTO
		PROPERTY Pages  AS USUAL AUTO
		PROPERTY PageWidth  AS USUAL AUTO
		METHOD SetFocus() AS VOID STRICT
			//SELF:Focus()

		PROPERTY TabOrientation  AS LONG AUTO
		PROPERTY Tabs  AS LOGIC AUTO
		PROPERTY TabStretch  AS LONG AUTO
		PROPERTY TabStyle  AS LONG AUTO


	END CLASS
	END NAMESPACE      
