

BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS CommandGroup IMPLEMENTS IVFPControl, IVFPOwner, IVFPGroup
#include "VFPControl.xh"
#include "VFPContainer.xh"
#include "VFPGroup.xh"

		PROPERTY BorderColor AS LONG AUTO
		METHOD SetFocus() AS VOID STRICT
			SELF:Focus()


	END CLASS
	END NAMESPACE      