
// Class Line BaseClass   Line Class  Line
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Line  IMPLEMENTS IVFPControl
		// Common properties that all VFP Objects support
#include "VFPObject.xh"

#include "VFPControl.xh"
		// BorderColor, BorderWidth, LineSlant — implemented in Line.prg
		PROPERTY DrawMode    AS LONG   AUTO
		PROPERTY PolyPoints  AS USUAL  AUTO
		PROPERTY Rotation    AS USUAL  AUTO
		PROPERTY FontOutline AS LOGIC  AUTO
		PROPERTY FontShadow  AS LOGIC  AUTO

	END CLASS
	END NAMESPACE
