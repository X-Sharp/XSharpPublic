// Spinner.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP Spinner Control - Numeric input with up/down buttons
    /// Maps VFP Spinner properties and methods to WinForms NumericUpDown
    ///
    /// Implements: IVFPObject, IVFPControl
    /// Includes: VFPObject.xh, VFPProperties.xh, FontProperties.xh
    ///           ControlProperties.xh, ControlSource.xh
    ///
    /// Base Class: System.Windows.Forms.NumericUpDown
    /// Note: Uses Current's simpler NumericUpDown approach (not Next's custom Panel)
    /// </summary>
    PARTIAL CLASS Spinner INHERIT System.Windows.Forms.NumericUpDown IMPLEMENTS IVFPObject, IVFPControl

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include shared VFP property constants/helpers
        // ============================================================================
        #include "XSharp/VFPProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

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

        /// <summary>SpinnerHighValue - Maximum spinner value</summary>
        PROPERTY SpinnerHighValue AS LONG
            GET
                RETURN (LONG)SUPER:Maximum
            END GET
            SET
                SUPER:Maximum := VALUE
            END SET
        END PROPERTY

        /// <summary>SpinnerLowValue - Minimum spinner value</summary>
        PROPERTY SpinnerLowValue AS LONG
            GET
                RETURN (LONG)SUPER:Minimum
            END GET
            SET
                SUPER:Minimum := VALUE
            END SET
        END PROPERTY

        /// <summary>KeyboardHighValue - Keyboard maximum value (maps to Maximum)</summary>
        PROPERTY KeyboardHighValue AS LONG
            GET
                RETURN (LONG)SUPER:Maximum
            END GET
            SET
                SUPER:Maximum := VALUE
            END SET
        END PROPERTY

        /// <summary>KeyboardLowValue - Keyboard minimum value (maps to Minimum)</summary>
        PROPERTY KeyboardLowValue AS LONG
            GET
                RETURN (LONG)SUPER:Minimum
            END GET
            SET
                SUPER:Minimum := VALUE
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
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR()
            SUPER()
            SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
            SELF:BackColor := Color.Transparent
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "Spinner"
            SELF:_class := "Spinner"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:Size := Size{100, 24}

    END CLASS

END NAMESPACE
