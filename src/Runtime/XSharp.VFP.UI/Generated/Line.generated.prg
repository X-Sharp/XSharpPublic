
// Class Line BaseClass   Line Class  Line
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Line  IMPLEMENTS IVFPControl
		// Common properties that all VFP Objects support
#include "VFPObject.xh"

#include "VFPControl.xh"
		PROPERTY BorderColor AS LONG AUTO
		PROPERTY BorderWidth AS USUAL AUTO
		PROPERTY DrawMode AS LONG AUTO
		PROPERTY LineSlant AS USUAL AUTO
		PROPERTY PolyPoints AS USUAL AUTO
		PROPERTY Rotation AS USUAL AUTO
		METHOD SetFocus() AS VOID STRICT
			//SELF:Focus()

	END CLASS
	END NAMESPACE      
