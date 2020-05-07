// Interfaces.prg
// This file defgines a couple of interfaces that are used in the VO Compatible Unicode GUI Classes



/// <summary>
/// This interface is defined for all VO..Control classes 
/// </summary>
INTERFACE IVOControl
	PROPERTY Control            AS XSharp.VO.Control GET
	PROPERTY ControlProperties  AS VOControlProperties GET
	METHOD SetVisualStyle AS VOID STRICT
	METHOD SetOwner(Owner AS XSharp.VO.Control) AS VOID 

END INTERFACE

INTERFACE IVOControlInitialize INHERIT IVOControl
	METHOD Initialize AS VOID STRICT
END INTERFACE



INTERFACE IVOForm
	PROPERTY Window		AS XSharp.VO.Window GET
	PROPERTY Properties AS VOFormProperties GET
END INTERFACE

/// <summary>
/// This interface defines a couple of common properties and methods  for Controls and Windows 
/// </summary>
INTERFACE IGuiObject
	PROPERTY Origin		AS Point GET SET
	PROPERTY Size		AS Dimension GET SET
	PROPERTY Caption	AS STRING GET SET
	PROPERTY HyperLabel AS HyperLabel GET SET
	PROPERTY NameSym	AS SYMBOL GET
    PROPERTY __Handle   as IntPtr GET
	METHOD   Destroy()	AS USUAL CLIPPER
END INTERFACE





/// <summary>
/// This interface defines a couple of properties and methods for 'owners' of Controls, such as the window class
/// </summary>
INTERFACE IControlParent
	PROPERTY __Handle AS IntPtr  GET
	PROPERTY __Form AS VOForm GET

    METHOD __AddTool(oControl AS Control) AS LOGIC STRICT
	METHOD __ShowToolTip(oControl AS Control) AS VOID STRICT
	METHOD __AddAlign(oControl AS IGuiObject, iType AS USUAL) AS LOGIC  STRICT
	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL STRICT
	METHOD __SetupDataControl(oDC AS Control) AS VOID
	METHOD GetDlgItem(nID as LONG) as ResourceDialogItem
END INTERFACE	


