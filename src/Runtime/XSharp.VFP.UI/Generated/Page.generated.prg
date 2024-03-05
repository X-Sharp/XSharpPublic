
// Class Page  BaseClass   Page  Class  Page
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Page  IMPLEMENTS IVFPControl, IVFPOwner, IVFPGraphics
#include "VFPControl.xh"
		#include "VFPContainer.xh"

		PROPERTY PAGEORDER AS USUAL AUTO
		PROPERTY Picture AS STRING AUTO
		METHOD SetFocus() AS VOID STRICT
			//SELF:Focus()

	END CLASS
	END NAMESPACE      