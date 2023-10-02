//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Interfaces.prg
// This file defines a couple of interfaces that are used in the VO Compatible Unicode GUI Classes

using VOSDK := XSharp.VO.SDK
using SWF   := System.Windows.Forms
using SD    := System.Drawing
using System.Collections
using System.Collections.Generic

/// <summary>
/// Interface with common properties that both the listbox and combobox class have
/// </summary>
INTERFACE IBaseListBox
    PROPERTY SelectedIndex AS LONG GET SET
    PROPERTY Items AS IList GET
    METHOD FindStringExact(cSearch AS STRING, nStart AS INT) AS INT
    METHOD FindString(cSearch AS STRING, nStart AS INT) AS INT
END INTERFACE


interface IVOUIObject
    property ClientRectangle as SD.Rectangle get
    property DisplayRectangle as SD.Rectangle get
    property Dock    as SWF.DockStyle get set
    property Height as long get set
    property IsDisposed as logic get
    property Location  as SD.Point get set
    property Size as SD.Size get set
    property Text as string get set
    property Visible  as logic get set
    property Width as long get set
    method PerformLayout as void strict
    method ResumeLayout as void strict
    method ResumeLayout(performLayout as logic) as void strict
    method SuspendLayout as void strict
end interface

/// <summary>
/// This interface declares the link between a Windows Forms Control and the matching VOSDK.Control
/// </summary>
interface IVOControlProperties inherit IVOControl
    /// <summary>
    /// The VOSDK.Control object that this Windows Forms Control is linked to
    /// </summary>
    property Control            as VOSDK.Control get
    /// <summary>
    /// An object with some properties, such as the Control, its owning window, style, exstyle etc.
    /// </summary>
    property ControlProperties  as VOControlProperties get
    /// <summary>
    /// This method is used to change the Visual Style of the control to match with the definition of the
    /// original VO control
    /// </summary>
    method SetVisualStyle() as void strict
    /// <summary>
    /// This method is called to link the Windows.Forms control to the VO Control
    /// </summary>
    /// <param name="Owner">The Owning VO Control</param>
    method SetOwner(Owner as VOSDK.Control) as void
end interface

/// <summary>
/// This interface is defined for all VO..Control classes
/// </summary>
interface IVOControl inherit IVOUIObject

    // Properties and methods implemented in SWF.Control that we depend on
    property AccessibleDescription as string get set
    property AccessibleName as string get set
    property Anchor as SWF.AnchorStyles get set
    property BackColor as SD.Color get set
    property Controls as SWF.Control.ControlCollection get
    property Cursor as SWF.Cursor get set
    property Enabled as logic get set
    property Focused as logic get

    property Font as SD.Font get set
    property ForeColor as SD.Color get set
    property Handle as IntPtr get
    property IsHandleCreated as logic get
    property Margin as SWF.Padding get set
    property Parent as SWF.Control get set
    property TabIndex as long get set
    property TabStop  as logic get set
    property Tag  as object get set

    method BringToFront() as void strict
    method Focus() as logic strict
    method Invalidate() as void strict
    method SendToBack() as void strict
    method Show() as void strict

    event Click  as EventHandler
    event GotFocus  as EventHandler
    event HandleCreated   as EventHandler
    event HandleDestroyed as EventHandler
    event KeyDown as SWF.KeyEventHandler
    event KeyPress as SWF.KeyPressEventHandler
    event KeyUp as SWF.KeyEventHandler
    event MouseClick as SWF.MouseEventHandler
    event PreviewKeyDown as SWF.PreviewKeyDownEventHandler
    event Resize as EventHandler
    event VisibleChanged as EventHandler

end interface

/// <summary>
/// This interface is implemented by controls that need special initialization, such as the TabControl and RTF Control
/// </summary>
interface IVOControlInitialize inherit IVOControlProperties
    method Initialize as void strict
end interface


interface IVOControlContainer inherit IVOUIObject
    property AllowDrop as logic get set
    method AddControl(oCtrl as System.Windows.Forms.Control) as void
    method SetChildIndex(oCtrl as IVOControl, nIndex as long) as void

end interface


interface IVOPanel inherit IVOControl,IVOControlContainer
    property AutoScroll as logic get set
    method CleanUp() as void strict
    method Dispose() as void strict
    method Prepare() as void strict
end interface

interface IVOFramePanel inherit IVOPanel
    property DefinedSize    as SD.Size get set
    property IsSubForm      as logic get set
end interface


interface IVOForm inherit IVOControlContainer, IVOUIObject
    property Window		as VOSDK.Window get
    property Properties as VOFormProperties get
end interface

/// <summary>
/// This interface defines a couple of common properties and methods  for Controls and Windows
/// </summary>
interface IGuiObject
    property Origin		as Point get set
    property Size		as Dimension get set
    property Caption	as string get set
    property HyperLabel as HyperLabel get set
    property NameSym	as symbol get
    property __Handle   as IntPtr get
    method   Destroy()	as usual clipper
#ifndef DOCUMENTATION
    method   Show()     as void clipper
#endif
    method   Hide()     as void strict
    method   SetFocus() as void strict

end interface





/// <summary>
/// This interface defines a couple of properties and methods for 'owners' of Controls, such as the window class
/// </summary>
interface IControlParent
    property __Handle as IntPtr  get
    property __Form as VOForm get

    method __AddTool(oControl as Control) as logic strict
    method __ShowToolTip(oControl as Control) as void strict
    method __AddAlign(oControl as IGuiObject, iType as usual) as logic  strict
    method ControlFocusChange(oControlFocusChangeEvent as  ControlFocusChangeEvent) as usual strict
    method __SetupDataControl(oDC as Control) as void
    method GetDlgItem(nID as long) as ResourceDialogItem
    property __Surface as IVOPanel get
end interface

interface IDataBrowser
    method Use(oServer as DataServer) as logic
    method __NotifyChanges(kNotify as dword) as usual
    property ContextMenu as Menu get set
    METHOD   Destroy()	AS USUAL CLIPPER
    method RestoreUpdate() as void strict
    method SuspendUpdate() as void strict
    property __Control as System.Windows.Forms.Control get
    method __Unlink(oDataServer := null_object as DataServer) as Control strict
end interface


interface IResource
    method Handle() as IntPtr strict
    method Destroy()	as usual clipper
        property Size as Dimension get
end interface
