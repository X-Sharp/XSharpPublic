
// Class PageFrame   BaseClass   Pageframe  Class  Pageframe
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS PageFrame   IMPLEMENTS IVFPControl, IVFPOwner
#include "VFPControl.xh"
#include "VFPContainer.xh"
#include "VFPGroup.xh"

		// ActivePage, PageCount, Pages, Tabs, TabOrientation, TabStyle — implemented in PageFrame.prg
		// TabStretch, PageHeight, PageWidth — implemented as real properties in PageFrame.prg
		// Resize/Moved events — implemented in PageFrame.prg
		PROPERTY BorderColor  AS LONG AUTO
		PROPERTY BorderWidth  AS LONG AUTO

	END CLASS
	END NAMESPACE
