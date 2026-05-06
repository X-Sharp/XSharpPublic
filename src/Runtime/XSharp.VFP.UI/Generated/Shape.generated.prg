
// Class Shape  BaseClass   Shape  Class  Shape
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS Shape IMPLEMENTS IVFPControl
#include "VFPObject.xh"
#include "VFPControl.xh"
// BorderColor, BorderWidth, Curvature, FillColor, FillStyle, Style — implemented in Shape.prg
PROPERTY DrawMode    AS LONG  AUTO
PROPERTY PolyPoints  AS USUAL AUTO
PROPERTY Rotation    AS USUAL AUTO
END CLASS
END NAMESPACE
