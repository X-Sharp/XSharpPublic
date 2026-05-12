
// Class Image  BaseClass   Image  Class  Image
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Image IMPLEMENTS IVFPControl
		#include "VFPControl.xh"

		PROPERTY BorderColor AS System.Drawing.Color AUTO
		PROPERTY BackStyle   AS INT   AUTO
		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow  AS LOGIC AUTO

	END CLASS
	END NAMESPACE