
// Class CommandButton    BaseClass   Commandbutton    Class  Commandbutton
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CommandButton     IMPLEMENTS IVFPControl
#include "VFPControl.xh"
    #include "VFPImage.xh"
    [System.ComponentModel.DefaultValue(false)];
        PROPERTY Cancel AS LOGIC AUTO

    [System.ComponentModel.DefaultValue(false)];
		PROPERTY Default AS LOGIC AUTO

    [System.ComponentModel.DefaultValue(0)];
        PROPERTY VisualEffect AS LONG AUTO

    [System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY WordWrap AS LOGIC AUTO
END CLASS
END NAMESPACE
