
// Class Image  BaseClass   Image  Class  Image
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Image IMPLEMENTS IVFPControl
#include "VFPControl.xh"
		PROPERTY BorderColor AS LONG AUTO
		// Picture - defined in Image.prg with custom implementation
		PROPERTY PictureVal AS USUAL AUTO
		PROPERTY RotateFlip AS LONG AUTO
		// Stretch - defined in Image.prg with custom implementation

	END CLASS
	END NAMESPACE