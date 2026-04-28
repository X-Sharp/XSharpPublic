// ListBox.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP ListBox Control - List selection control
    /// Maps VFP ListBox properties and methods to WinForms ListBox
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPList
    /// Includes: VFPList.xh, VFPObject.xh, ControlProperties.xh, ControlSource.xh
    ///
    /// Base Class: System.Windows.Forms.ListBox
    /// </summary>
    PARTIAL CLASS ListBox INHERIT System.Windows.Forms.ListBox IMPLEMENTS IVFPObject, IVFPControl, IVFPList

        // ============================================================================
        // Include shared VFP list control properties and methods
        // ============================================================================
        #include "Headers/VFPList.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include common VFP control properties and event wiring
        // ============================================================================
        #include "Headers/ControlProperties.xh"

        // ============================================================================
        // Include ControlSource data binding
        // ============================================================================
        #include "Headers/ControlSource.xh"

        // ============================================================================
        // PRIVATE FIELDS
        // ============================================================================

        PRIVATE _columnCount AS INT
        PRIVATE _columnWidths AS STRING
        PRIVATE _displayCount AS INT
        PRIVATE _multiSelect AS INT
        PRIVATE _dragMode AS INT
        PRIVATE _dragIcon AS STRING
        PRIVATE _baseClass AS STRING
        PRIVATE _class AS STRING
        PRIVATE _classLibrary AS STRING
        PRIVATE _comment AS STRING
        PRIVATE _helpContextID AS LONG
        PRIVATE _whatsThisHelpID AS LONG

        // ============================================================================
        // PROPERTIES
        // ============================================================================

        /// <summary>Value - Value of selected item</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.DesignerSerializationVisibility(System.ComponentModel.DesignerSerializationVisibility.Hidden)];
        PROPERTY Value AS USUAL
            GET
                IF SELF:SelectedIndex >= 0
                    RETURN SELF:Items[SELF:SelectedIndex]
                ENDIF
                RETURN NIL
            END GET
            SET
                IF !IsNil(VALUE)
                    SELF:SelectByValue(VALUE)
                ENDIF
            END SET
        END PROPERTY

        /// <summary>AutoHideScrollBar - Auto-hide scrollbar when not needed</summary>
        PROPERTY AutoHideScrollBar AS LONG AUTO

        /// <summary>NullDisplay - Display string for NULL value</summary>
        PROPERTY NullDisplay AS STRING AUTO

        /// <summary>Picture - Picture clause for formatting</summary>
        PROPERTY Picture AS STRING AUTO

        /// <summary>MultiSelect - Multi-selection mode (0=None, 1=Single, 2=Multiple)</summary>
        PROPERTY MultiSelect AS INT
            GET
                RETURN SELF:_multiSelect
            END GET
            SET
                SELF:_multiSelect := VALUE
                SWITCH VALUE
                    CASE 0
                        SELF:SelectionMode := SelectionMode.None
                    CASE 1
                        SELF:SelectionMode := SelectionMode.One
                    CASE 2
                        SELF:SelectionMode := SelectionMode.MultiSimple
                END SWITCH
            END SET
        END PROPERTY

        /// <summary>ColumnCount - Number of columns</summary>
        PROPERTY ColumnCount AS INT
            GET
                RETURN SELF:_columnCount
            END GET
            SET
                SELF:_columnCount := VALUE
            END SET
        END PROPERTY

        /// <summary>ColumnWidths - Column widths (comma-separated)</summary>
        PROPERTY ColumnWidths AS STRING
            GET
                RETURN SELF:_columnWidths
            END GET
            SET
                SELF:_columnWidths := VALUE
            END SET
        END PROPERTY

        /// <summary>BoundColumn - Which column to return as value (1-based)</summary>
        PROPERTY BoundColumn AS INT
            GET
                RETURN SELF:_boundColumn
            END GET
            SET
                IF VALUE > 0
                    SELF:_boundColumn := VALUE
                ENDIF
            END SET
        END PROPERTY

        /// <summary>DisplayCount - Number of visible items</summary>
        PROPERTY DisplayCount AS INT
            GET
                RETURN SELF:_displayCount
            END GET
            SET
                SELF:_displayCount := VALUE
            END SET
        END PROPERTY

        // ============================================================================
        // IVFPControl Implementation
        // ============================================================================

        [Category("VFP Behavior")];
        [Description("Drag icon path")];
        [DefaultValue("")];
        PROPERTY DragIcon AS STRING
            GET
                RETURN SELF:_dragIcon
            END GET
            SET
                SELF:_dragIcon := VALUE
            END SET
        END PROPERTY

        [Category("VFP Behavior")];
        [Description("Drag mode (0=manual, 1=automatic)")];
        [DefaultValue(0)];
        PROPERTY DragMode AS LONG
            GET
                RETURN SELF:_dragMode
            END GET
            SET
                SELF:_dragMode := VALUE
            END SET
        END PROPERTY

        PUBLIC METHOD Drag(nAction) AS USUAL CLIPPER
            RETURN NIL
        END METHOD

        PUBLIC METHOD SetFocus() AS VOID STRICT
            SELF:Focus()
        END METHOD

        // ============================================================================
        // METHODS
        // ============================================================================

        /// <summary>AddItem - Add item to list</summary>
        PUBLIC METHOD AddItem(item AS STRING) AS VOID
            SELF:Items:Add(item)
        END METHOD

        /// <summary>RemoveItem - Remove item by index (1-based)</summary>
        PUBLIC METHOD RemoveItem(index AS INT) AS VOID
            IF index > 0 .AND. index <= SELF:Items:Count
                SELF:Items:RemoveAt(index - 1)
            ENDIF
        END METHOD

        /// <summary>Clear - Clear all items</summary>
        PUBLIC METHOD Clear() AS VOID
            SELF:Items:Clear()
            SELF:_vfpValue := NIL
        END METHOD

        /// <summary>FindString - Find item by prefix (1-based, 0=not found)</summary>
        PUBLIC METHOD FindString(searchStr AS STRING) AS INT
            VAR idx := SUPER:FindString(searchStr)
            IF idx >= 0
                RETURN idx + 1
            ENDIF
            RETURN 0
        END METHOD

        /// <summary>FindStringExact - Find exact match (1-based, 0=not found)</summary>
        PUBLIC METHOD FindStringExact(searchStr AS STRING) AS INT
            VAR idx := SUPER:FindStringExact(searchStr)
            IF idx >= 0
                RETURN idx + 1
            ENDIF
            RETURN 0
        END METHOD

        /// <summary>EnsureVisible - Scroll item into view (1-based)</summary>
        PUBLIC METHOD EnsureVisible(index AS INT) AS VOID
            IF index > 0 .AND. index <= SELF:Items:Count
                SELF:TopIndex := index - 1
            ENDIF
        END METHOD

        /// <summary>Select item by value</summary>
        PRIVATE METHOD SelectByValue(value AS USUAL) AS VOID
            FOR VAR i := 0 TO SELF:Items:Count - 1
                VAR item := SELF:Items[i]
                IF item:ToString() == value:ToString()
                    SELF:SelectedIndex := i
                    EXIT
                ENDIF
            NEXT
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:_rowSource := ""
            SELF:_rowSourceType := 0
            SELF:_boundColumn := 1
            SELF:_listIndex := -1
            SELF:_columnCount := 1
            SELF:_columnWidths := ""
            SELF:_displayCount := 0
            SELF:_multiSelect := 0
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "ListBox"
            SELF:_class := "ListBox"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:SelectionMode := SelectionMode.One
            SELF:Size := Size{100, 170}

    END CLASS

END NAMESPACE
