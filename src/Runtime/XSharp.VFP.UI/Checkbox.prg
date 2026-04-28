// CheckBox.prg
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
    /// VFP CheckBox Control - Boolean selection with caption
    /// Maps VFP CheckBox properties and methods to WinForms CheckBox
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPButton
    /// Includes: VFPButton.xh (shared button control code)
    ///           ControlProperties.xh (VFP control event wiring)
    ///           ControlSource.xh (data binding)
    ///
    /// Base Class: System.Windows.Forms.CheckBox
    /// </summary>
    PARTIAL CLASS CheckBox INHERIT System.Windows.Forms.CheckBox IMPLEMENTS IVFPObject, IVFPControl, IVFPButton

        // ============================================================================
        // Include shared VFP button control properties and methods
        // ============================================================================
        #include "Headers/VFPButton.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include common VFP control properties and event wiring
        // ============================================================================
        #include "Headers/ControlProperties.xh"

        // ============================================================================
        // Include ControlSource data binding support
        // ============================================================================
        #include "Headers/ControlSource.xh"

        // ============================================================================
        // PRIVATE FIELDS - Additional CheckBox-specific backing storage
        // ============================================================================

        PRIVATE _dragMode AS INT
        PRIVATE _dragIcon AS STRING
        PRIVATE _baseClass AS STRING
        PRIVATE _class AS STRING
        PRIVATE _classLibrary AS STRING
        PRIVATE _comment AS STRING
        PRIVATE _helpContextID AS LONG
        PRIVATE _whatsThisHelpID AS LONG

        // ============================================================================
        // PROPERTIES - VFP CheckBox API
        // ============================================================================

        /// <summary>Value - Checkbox value (TRUE/FALSE/NIL for indeterminate)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.Description("Checkbox value")];
        [System.ComponentModel.DesignerSerializationVisibility(System.ComponentModel.DesignerSerializationVisibility.Hidden)];
        PROPERTY Value AS USUAL
            GET
                IF SELF:CheckState == CheckState.Checked
                    RETURN TRUE
                ELSEIF SELF:CheckState == CheckState.Unchecked
                    RETURN FALSE
                ELSE
                    RETURN NIL
                ENDIF
            END GET
            SET
                IF VALUE == TRUE
                    SELF:CheckState := CheckState.Checked
                ELSEIF VALUE == FALSE
                    SELF:CheckState := CheckState.Unchecked
                ELSEIF IsNil(VALUE)
                    IF SELF:ThreeState
                        SELF:CheckState := CheckState.Indeterminate
                    ELSE
                        SELF:CheckState := CheckState.Unchecked
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        /// <summary>Centered - Whether text is centered</summary>
        PROPERTY Centered AS LOGIC AUTO

        /// <summary>ReadOnly - Whether the checkbox is read-only</summary>
        PROPERTY ReadOnly AS LOGIC AUTO

        /// <summary>WordWrap - Whether caption word-wraps</summary>
        PROPERTY WordWrap AS LOGIC AUTO

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
        // METHODS - VFP CheckBox Methods
        // ============================================================================

        /// <summary>Click - Programmatically trigger click</summary>
        PUBLIC METHOD ClickBox() AS VOID
            SELF:PerformClick()
        END METHOD

        // ============================================================================
        // EVENT HANDLERS
        // ============================================================================

        /// <summary>Update value when check state changes</summary>
        PROTECTED OVERRIDE METHOD OnCheckedChanged(e AS System.EventArgs) AS VOID
            SUPER:OnCheckedChanged(e)
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            // Initialize backing fields for VFP properties
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "CheckBox"
            SELF:_class := "CheckBox"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:Checked := FALSE
            SELF:ThreeState := FALSE
            SELF:AutoSize := TRUE
            SELF:Size := Size{100, 17}
        END CONSTRUCTOR

    END CLASS

END NAMESPACE
