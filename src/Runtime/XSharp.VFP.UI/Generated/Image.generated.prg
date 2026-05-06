
// Class Image  BaseClass   Image  Class  Image
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Image IMPLEMENTS IVFPControl
#include "VFPControl.xh"
		PROPERTY BorderColor AS System.Drawing.Color AUTO
		// Picture and Stretch are implemented in Image.prg with real logic
		PROPERTY PictureVal AS USUAL AUTO
		// RotateFlip is implemented with real logic in Image.prg

	END CLASS
	END NAMESPACE      