
// Class FormSet  BaseClass   Formset  Class  Formset
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS FormSet IMPLEMENTS IVFPObject
#define VFP_CONTROLCOUNT_OVERRIDE
#define NOSET
#include "VFPContainer.xh"
#include "VFPPropertiesDynamic.xh"
PROPERTY AutoRelease AS LOGIC AUTO
PROPERTY BufferMode AS LONG AUTO
PROPERTY DataSession AS USUAL AUTO
PROPERTY DataSessionID  AS USUAL AUTO
[Obsolete];
PROPERTY ReadCycle AS USUAL AUTO
[Obsolete];
PROPERTY ReadLock AS USUAL AUTO
[Obsolete];
PROPERTY ReadMouse  AS USUAL AUTO
[Obsolete];
PROPERTY ReadSave AS USUAL AUTO
[Obsolete];
PROPERTY ReadTimeout  AS USUAL AUTO
// Release implemented in FormSet.prg
METHOD SaveAs(cFileName, oDataEnvironment) AS USUAL CLIPPER

RETURN NIL
[Obsolete];
PROPERTY WindowList  AS USUAL AUTO
PROPERTY WindowType  AS LONG AUTO
END CLASS
END NAMESPACE      
