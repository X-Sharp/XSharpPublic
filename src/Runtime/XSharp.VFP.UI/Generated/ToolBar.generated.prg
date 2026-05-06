
// Class ToolBar BaseClass   Toolbar Class  Toolbar
BEGIN NAMESPACE XSharp.VFP.UI
PARTIAL CLASS ToolBar IMPLEMENTS IVFPObject, IVFPOwner
#include "VFPObject.xh"
// Suppress the generic AddObject from VFPContainer.xh — ToolBar.prg provides its own override
#define NO_VFPCONTAINER_ADDOBJECT
	#include "VFPContainer.xh"
	#include "VFPPropertiesDynamic.xh"
// Wire standard VFP events: Move(), SetFocus(), Valid, When, GotFocus, etc.
// ControlProperties.xh included in ToolBar.prg (requires USING System.ComponentModel)

// hWnd, Movable, Sizable, KeyPreview, LockScreen, Release — implemented in ToolBar.prg
PROPERTY CONTROLBOX       AS USUAL AUTO
PROPERTY DataSession       AS LONG  AUTO
PROPERTY DataSessionID     AS USUAL AUTO
PROPERTY HelpContextID     AS LONG  AUTO
PROPERTY MouseIcon         AS STRING AUTO
METHOD OLEDrag AS USUAL CLIPPER
RETURN NIL
PROPERTY OLEDragMode       AS LONG  AUTO
PROPERTY OLEDragPicture    AS STRING AUTO
PROPERTY OLEDropEffects    AS LONG  AUTO
PROPERTY OLEDropHasData    AS LONG  AUTO
PROPERTY OLEDropMode       AS LONG  AUTO
PROPERTY ScaleMode         AS USUAL AUTO
PROPERTY Themes            AS LOGIC AUTO
PROPERTY WhatsThisHelpID   AS LONG  AUTO
END CLASS
END NAMESPACE
