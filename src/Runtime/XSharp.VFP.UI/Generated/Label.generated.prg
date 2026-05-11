
// Class Label BaseClass   Label Class  Label
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Label  IMPLEMENTS IVFPControl,IVFPText
		#include "VFPControl.xh"

		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow  AS LOGIC AUTO
		PROPERTY BorderColor AS System.Drawing.Color AUTO

	END CLASS
END NAMESPACE