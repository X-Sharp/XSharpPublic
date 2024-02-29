
// Class CommandButton    BaseClass   Commandbutton    Class  Commandbutton
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CommandButton     IMPLEMENTS IVFPControl
#include "VFPControl.xh"
		#include "VFPImage.xh"
		PROPERTY Cancel AS LOGIC AUTO
		PROPERTY Default AS LOGIC AUTO

		PROPERTY VisualEffect AS LONG AUTO
		PROPERTY WordWrap AS LOGIC AUTO
END CLASS
END NAMESPACE      