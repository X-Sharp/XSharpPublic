
// Class Shape  BaseClass   Shape  Class  Shape
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS Shape IMPLEMENTS IVFPObject
#include "VFPObject.xh"
PROPERTY BorderColor AS LONG AUTO
PROPERTY BORDERWIDTH AS USUAL AUTO
PROPERTY CURVATURE AS USUAL AUTO
METHOD DRAG AS USUAL CLIPPER
    RETURN NIL

    [System.ComponentModel.DefaultValue("")];
        PROPERTY DragIcon AS USUAL AUTO

    [System.ComponentModel.DefaultValue(0)];
        PROPERTY DragMode AS LONG AUTO

PROPERTY DrawMode AS LONG AUTO
PROPERTY FillColor AS LONG AUTO
PROPERTY FillStyle AS USUAL AUTO
PROPERTY HelpContextID AS LONG AUTO
PROPERTY MouseIcon AS STRING AUTO
METHOD OLEDrag AS USUAL CLIPPER
    RETURN NIL

[System.ComponentModel.DefaultValue(0)];
PROPERTY OLEDragMode AS LONG AUTO

[System.ComponentModel.DefaultValue(NULL)];
PROPERTY OLEDragPicture AS STRING AUTO

[System.ComponentModel.DefaultValue(0)];
PROPERTY OLEDropEffects AS LONG AUTO

[System.ComponentModel.DefaultValue(0)];
PROPERTY OLEDropHasData AS LONG AUTO

[System.ComponentModel.DefaultValue(0)];
PROPERTY OLEDropMode AS LONG AUTO

PROPERTY PolyPoints AS USUAL AUTO
PROPERTY Rotation AS USUAL AUTO
PROPERTY Style AS LONG AUTO
PROPERTY WhatsThisHelpID AS LONG AUTO
END CLASS
END NAMESPACE      
