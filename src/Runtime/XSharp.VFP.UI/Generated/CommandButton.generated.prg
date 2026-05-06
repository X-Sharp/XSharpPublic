// Class CommandButton    BaseClass   Commandbutton    Class  Commandbutton
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS CommandButton     IMPLEMENTS IVFPControl
#include "VFPControl.xh"
// PicturePosition implemented as real property in CommandButton.prg — suppress AUTO stub
#define NO_PICTUREPOSITION
    #include "VFPImage.xh"
    // Cancel and Default implemented in CommandButton.prg — wired to Form.CancelButton / AcceptButton

    [System.ComponentModel.DefaultValue(0)];
        PROPERTY VisualEffect AS LONG AUTO

    // WordWrap implemented as real property in CommandButton.prg

    // DisabledBackColor and DisabledForeColor already declared in VFPImage.xh — do not re-declare
END CLASS
END NAMESPACE
