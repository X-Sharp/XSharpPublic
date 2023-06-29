// Interfaces.prg
// This file defgines a couple of interfaces that are used in the VO Compatible Unicode GUI Classes

USING VOSDK := XSharp.VO.SDK
USING SWF   := System.Windows.Forms
USING SD    := System.Drawing
USING System.Collections
USING System.Collections.Generic


INTERFACE IVOUIObject
    PROPERTY ClientRectangle AS SD.Rectangle GET
    PROPERTY DisplayRectangle AS SD.Rectangle GET
    PROPERTY Dock    AS SWF.DockStyle GET SET
    PROPERTY Height AS LONG GET SET
    PROPERTY IsDisposed AS LOGIC GET
    PROPERTY Location  AS SD.Point GET SET
    PROPERTY Size AS SD.Size GET SET
    PROPERTY Text AS STRING GET SET
    PROPERTY Visible  AS LOGIC GET SET
    PROPERTY Width AS LONG GET SET
    METHOD PerformLayout AS VOID STRICT
    METHOD ResumeLayout AS VOID STRICT
    METHOD ResumeLayout(performLayout AS LOGIC) AS VOID STRICT
    METHOD SuspendLayout AS VOID STRICT
END INTERFACE


INTERFACE IVOControlProperties
	PROPERTY Control            AS VOSDK.Control GET
	PROPERTY ControlProperties  AS VOControlProperties GET
	METHOD SetVisualStyle AS VOID STRICT
	METHOD SetOwner(Owner AS VOSDK.Control) AS VOID
END INTERFACE

/// <summary>
/// This interface is defined for all VO..Control classes
/// </summary>
INTERFACE IVOControl INHERIT IVOUIObject
	// Properties and methods implemented in SWF.Control that we depend on
    PROPERTY AccessibleDescription AS STRING GET SET
    PROPERTY AccessibleName AS STRING GET SET
    PROPERTY Anchor AS SWF.AnchorStyles GET SET
    PROPERTY BackColor AS SD.Color GET SET
    PROPERTY Controls AS SWF.Control.ControlCollection GET
    PROPERTY Cursor AS SWF.Cursor GET SET
    PROPERTY Enabled AS LOGIC GET SET
    PROPERTY Focused AS LOGIC GET

    PROPERTY Font AS SD.Font GET SET
    PROPERTY ForeColor AS SD.Color GET SET
    PROPERTY Handle AS IntPtr GET
    PROPERTY IsHandleCreated AS LOGIC GET
    PROPERTY Margin AS SWF.Padding GET SET
    PROPERTY Parent AS SWF.Control GET SET
    PROPERTY TabIndex AS LONG GET SET
    PROPERTY TabStop  AS LOGIC GET SET
    PROPERTY Tag  AS OBJECT GET SET

    METHOD BringToFront() AS VOID STRICT
    METHOD Focus() AS LOGIC STRICT
    METHOD Invalidate() AS VOID STRICT
    METHOD SendToBack() AS VOID STRICT
    METHOD Show() AS VOID STRICT

    EVENT Click  AS EventHandler
    EVENT GotFocus  AS EventHandler
    EVENT HandleCreated   AS EventHandler
    EVENT HandleDestroyed AS EventHandler
    EVENT KeyDown AS SWF.KeyEventHandler
    EVENT KeyPress AS SWF.KeyPressEventHandler
    EVENT KeyUp AS SWF.KeyEventHandler
    EVENT MouseClick AS SWF.MouseEventHandler
    EVENT PreviewKeyDown AS SWF.PreviewKeyDownEventHandler
    EVENT Resize AS EventHandler
    EVENT VisibleChanged AS EventHandler

END INTERFACE

INTERFACE IVOControlInitialize INHERIT IVOControlProperties
	METHOD Initialize AS VOID STRICT
END INTERFACE


INTERFACE IVOControlContainer INHERIT IVOUIObject
    PROPERTY AllowDrop AS LOGIC GET SET
    PROPERTY BackColor AS SD.Color GET SET
    PROPERTY Controls AS SWF.Control.ControlCollection GET
    METHOD AddControl(oCtrl AS IVOControl) AS VOID
    METHOD SetChildIndex(oCtrl AS IVOControl, nIndex AS LONG) AS VOID

END INTERFACE


INTERFACE IVOPanel INHERIT IVOControl,IVOControlContainer
    PROPERTY AutoScroll AS LOGIC GET SET
    METHOD CleanUp() AS VOID STRICT
    METHOD Dispose() AS VOID STRICT
    METHOD Prepare() AS VOID STRICT
END INTERFACE

INTERFACE IVOFramePanel INHERIT IVOPanel
    PROPERTY DefinedSize    AS SD.Size GET SET
    PROPERTY IsSubForm      AS LOGIC GET SET
END INTERFACE


INTERFACE IVOForm INHERIT IVOControlContainer, IVOUIObject
	PROPERTY Window		AS VOSDK.Window GET
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
    PROPERTY __Handle   AS IntPtr GET
	METHOD   Destroy()	AS USUAL
    METHOD   Show()     AS VOID STRICT
    METHOD   Hide()     AS VOID STRICT
    METHOD   SetFocus() AS VOID STRICT

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
	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS VOID STRICT
	METHOD __SetupDataControl(oDC AS Control) AS VOID
	METHOD GetDlgItem(nID as LONG) as ResourceDialogItem
END INTERFACE
