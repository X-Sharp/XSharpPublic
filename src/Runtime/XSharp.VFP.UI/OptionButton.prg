// OptionButton.prg
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
    /// VFP OptionButton Control - Radio button for mutually exclusive selection
    /// Maps VFP OptionButton properties and methods to WinForms RadioButton
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPButton
    /// Includes: VFPButton.xh, ControlProperties.xh, FontProperties.xh, ControlSource.xh
    ///
    /// Base Class: System.Windows.Forms.RadioButton
    /// </summary>
    PARTIAL CLASS OptionButton INHERIT System.Windows.Forms.RadioButton IMPLEMENTS IVFPObject, IVFPControl, IVFPButton

        // ============================================================================
        // Include shared VFP button control properties and methods
        // ============================================================================
        #include "Headers/VFPButton.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include VFPProperties (shared VFP property constants/helpers)
        // ============================================================================
        #include "XSharp/VFPProperties.xh"

        // ============================================================================
        // Include common VFP control properties and event wiring
        // ============================================================================
        #include "Headers/ControlProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

        // ============================================================================
        // Include ControlSource data binding support
        // ============================================================================
        #include "Headers/ControlSource.xh"

        // ============================================================================
        // PRIVATE FIELDS
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
        // PROPERTIES
        // ============================================================================

        /// <summary>Value - Radio button value (TRUE if selected)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.Description("Radio button value")];
        [System.ComponentModel.DesignerSerializationVisibility(System.ComponentModel.DesignerSerializationVisibility.Hidden)];
        PROPERTY Value AS LOGIC
            GET
                RETURN SELF:Checked
            END GET
            SET
                SELF:Checked := VALUE
                IF VALUE .AND. SELF:Parent != NULL
                    FOREACH VAR ctl IN SELF:Parent:Controls
                        IF ctl != SELF .AND. ctl IS RadioButton
                            VAR rb := (RadioButton)ctl
                            rb:Checked := FALSE
                        ENDIF
                    NEXT
                ENDIF
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

        /// <summary>Click - Programmatically trigger click</summary>
        PUBLIC METHOD ClickOption() AS VOID
            SELF:PerformClick()
        END METHOD

        // ============================================================================
        // EVENT HANDLERS
        // ============================================================================

        /// <summary>Update value and manage mutual exclusion when checked</summary>
        PROTECTED OVERRIDE METHOD OnCheckedChanged(e AS System.EventArgs) AS VOID
            SUPER:OnCheckedChanged(e)
            IF SELF:Checked .AND. SELF:Parent != NULL
                FOREACH VAR ctl IN SELF:Parent:Controls
                    IF ctl != SELF .AND. ctl IS RadioButton
                        VAR rb := (RadioButton)ctl
                        rb:Checked := FALSE
                    ENDIF
                NEXT
            ENDIF
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "OptionButton"
            SELF:_class := "OptionButton"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:Checked := FALSE
            SELF:AutoCheck := TRUE
            SELF:AutoSize := TRUE
            SELF:Size := Size{10, 16}

    END CLASS

END NAMESPACE
