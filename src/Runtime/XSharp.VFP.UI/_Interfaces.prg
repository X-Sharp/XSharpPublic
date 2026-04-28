//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// VFP Control Framework Interfaces
// Defines the contracts that all VFP controls must implement

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Interface for most objects in the VFP UI
    /// Base interface that all VFP objects should implement
    /// </summary>
    INTERFACE IVFPObject
        PROPERTY BaseClass AS STRING GET
        PROPERTY Class AS STRING GET
        PROPERTY ClassLibrary AS STRING GET
        PROPERTY Comment AS STRING GET

        METHOD ReadExpression(cPropertyName AS STRING) AS STRING
        METHOD WriteExpression(cPropertyName AS STRING, cExpression AS STRING) AS STRING
        METHOD ReadMethod(cMethod AS STRING) AS STRING
        METHOD WriteMethod(cMethodName AS STRING, cMethodText AS STRING, lCreateMethod AS LOGIC, nVisibility AS LONG, cDescription AS STRING) AS VOID
        METHOD ResetToDefault(cPropertyName AS STRING) AS VOID
    END INTERFACE

    /// <summary>
    /// Interface for objects that can contain a picture/graphic
    /// Used by Image, CommandButton, CheckBox and other graphical controls
    /// </summary>
    INTERFACE IVFPGraphics
        PROPERTY Picture AS STRING GET SET
    END INTERFACE

    /// <summary>
    /// Interface for text-related visual properties
    /// Used by controls that display or edit text
    /// </summary>
    INTERFACE IVFPText
        PROPERTY DisabledBackColor AS LONG GET SET
        PROPERTY DisabledForeColor AS LONG GET SET
    END INTERFACE

    /// <summary>
    /// Interface for objects that can contain images
    /// Inherits IVFPGraphics and IVFPText for complete image support
    /// Used by Image control and button controls with pictures
    /// </summary>
    INTERFACE IVFPImage INHERIT IVFPGraphics, IVFPText
        PROPERTY DisabledPicture AS STRING GET SET
        PROPERTY DownPicture AS STRING GET SET
        PROPERTY PictureMargin AS LONG GET SET
        PROPERTY PicturePosition AS LONG GET SET
        PROPERTY PictureSpacing AS LONG GET SET
    END INTERFACE

    /// <summary>
    /// Interface for objects that support Help context
    /// Used by all interactive controls
    /// </summary>
    INTERFACE IVFPHelp
        PROPERTY HelpContextID AS LONG GET SET
        PROPERTY WhatsThisHelpID AS LONG GET SET
    END INTERFACE

    /// <summary>
    /// Interface for interactive controls that can be dragged
    /// Base interface for most UI controls (TextBox, ListBox, Button, etc.)
    /// </summary>
    INTERFACE IVFPControl INHERIT IVFPObject, IVFPHelp
        METHOD Drag(nAction) AS USUAL CLIPPER
        PROPERTY DragIcon AS STRING GET SET
        PROPERTY DragMode AS LONG GET SET
        METHOD SetFocus() AS VOID STRICT
    END INTERFACE

    /// <summary>
    /// Interface for button-based controls (CommandButton, CheckBox, OptionButton)
    /// BUG-02 FIX: This interface was missing — referenced in class declarations but never defined.
    /// </summary>
    INTERFACE IVFPButton INHERIT IVFPControl
        PROPERTY Caption AS STRING GET SET
        PROPERTY Value AS USUAL GET SET
        PROPERTY Alignment AS INT GET SET
        METHOD _Click() AS VOID STRICT
    END INTERFACE

    /// <summary>
    /// Interface for controls that can contain and manage other controls
    /// Used by Form, Container, PageFrame, Page, and group controls
    /// </summary>
    INTERFACE IVFPOwner INHERIT IVFPObject
		METHOD AddObject(cName, cClass , cOLEClass , aInit1, aInit2 ) AS USUAL CLIPPER
		METHOD NewObject(cObjectName, cClassName , cModule , cInApplication, eParameter1, eParameter2) AS USUAL CLIPPER
		METHOD RemoveObject(cObjectName AS STRING) AS LOGIC
        PROPERTY Objects AS System.Collections.ICollection GET
        PROPERTY ControlCount AS LONG GET SET
    END INTERFACE

    /// <summary>
    /// Interface for controls that manage lists of items
    /// Used by ListBox and ComboBox controls
    /// </summary>
    INTERFACE IVFPList INHERIT IVFPControl
        // Data source properties
        PROPERTY RowSource AS STRING GET SET
        PROPERTY RowSourceType AS LONG GET SET
        PROPERTY BoundColumn AS LONG GET SET
        PROPERTY BoundTo AS LOGIC GET SET
        PROPERTY DisplayValue AS USUAL GET SET

        // Item management methods
		METHOD AddItem(cItem , nIndex , nColumn) AS VOID  CLIPPER
		METHOD AddListItem(cItem , nIndex , nColumn) AS VOID  CLIPPER
        METHOD RemoveItem(nIndex AS LONG) AS VOID
        METHOD RemoveListItem(nIndex AS LONG) AS VOID
        METHOD Clear() AS VOID CLIPPER
        METHOD Requery() AS VOID STRICT

        // List content properties
        PROPERTY List AS ARRAY GET SET
        PROPERTY ListCount AS LONG GET
        PROPERTY ListIndex AS LONG GET SET
        PROPERTY ListItemID AS LONG GET SET
        PROPERTY FirstElement AS LONG GET SET

        // Item data properties
        PROPERTY ItemData AS LONG GET SET
        PROPERTY ItemBackColor AS LONG GET SET
        PROPERTY ItemForeColor AS LONG GET SET
        PROPERTY ItemTips AS LOGIC GET SET
        PROPERTY SelectedItemBackColor AS LONG GET SET
        PROPERTY SelectedItemForeColor AS LONG GET SET

        // Item ID methods
        METHOD IndexToItemID(nIndex AS LONG) AS LONG
        METHOD ItemIDToIndex(nItemId AS LONG) AS LONG
    END INTERFACE

    /// <summary>
    /// Interface for group controls (OptionGroup, CommandGroup)
    /// Controls that contain multiple related child controls
    /// </summary>
    INTERFACE IVFPGroup INHERIT IVFPObject
        PROPERTY MemberClass AS STRING GET SET
        PROPERTY MemberClassLibrary AS STRING GET SET
    END INTERFACE

    /// <summary>
    /// Interface for editable controls
    /// Used by TextBox, EditBox, and other text input controls
    /// </summary>
    INTERFACE IVFPEditable
        PROPERTY SelectOnEntry AS LOGIC GET SET
    END INTERFACE

    /// <summary>
    /// Interface for controls with borders and appearance
    /// Used by most visual controls
    /// </summary>
    INTERFACE IVFPBorder
        PROPERTY BorderColor AS System.Drawing.Color GET SET
        PROPERTY BorderStyle AS INT GET SET
        PROPERTY BorderWidth AS INT GET SET
    END INTERFACE

    /// <summary>
    /// Interface for controls that display column-based data
    /// Used by Grid control and column-related controls
    /// </summary>
    INTERFACE IVFPGridControl INHERIT IVFPControl
        PROPERTY ColumnCount AS LONG GET SET
        PROPERTY Columns AS System.Collections.IList GET
        PROPERTY HeaderHeight AS LONG GET SET
        PROPERTY RecordCount AS LONG GET
        PROPERTY RecordSource AS STRING GET SET
    END INTERFACE

END NAMESPACE // XSharp.VFP.UI
