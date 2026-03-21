
// Class CommandButton    BaseClass   Commandbutton    Class  Commandbutton
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CommandButton     IMPLEMENTS IVFPControl
#include "VFPControl.xh"
    #include "VFPImage.xh"
    // Cancel - defined in CommandButton.prg with custom implementation
    // Default - defined in CommandButton.prg with custom implementation

    [System.ComponentModel.DefaultValue(0)];
        PROPERTY VisualEffect AS LONG AUTO

    [System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY WordWrap AS LOGIC AUTO
END CLASS
END NAMESPACE
