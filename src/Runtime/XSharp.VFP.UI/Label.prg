// Label.prg
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
    /// VFP Label Control - Static text display
    /// Maps VFP Label properties and methods to WinForms Label
    ///
    /// Implements: IVFPObject, IVFPControl
    /// Includes: VFPObject.xh, VFPProperties.xh, TextControlProperties.xh, FontProperties.xh
    ///
    /// Base Class: System.Windows.Forms.Label
    /// </summary>
    PARTIAL CLASS Label INHERIT System.Windows.Forms.Label IMPLEMENTS IVFPObject, IVFPControl

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include shared VFP property constants/helpers (VFPAlignmentConvert etc.)
        // ============================================================================
        #include "XSharp/VFPProperties.xh"

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

        /// <summary>Alignment - Text alignment using VFP convention</summary>
        PROPERTY Alignment AS INT
            GET
                RETURN VFPAlignmentConvert(SELF:TextAlign)
            END GET
            SET
                SELF:TextAlign := VFPAlignmentConvert(VALUE)
            END SET
        END PROPERTY

        /// <summary>Style - VFP label style (placeholder)</summary>
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
        PROPERTY Style AS INT AUTO

        /// <summary>Rotation - Text rotation in degrees</summary>
        PROPERTY Rotation AS INT AUTO

        /// <summary>DisabledBackColor - Background color when disabled</summary>
        [System.ComponentModel.DefaultValue(0)];
        PROPERTY DisabledBackColor AS LONG AUTO

        /// <summary>DisabledForeColor - Foreground color when disabled</summary>
        [System.ComponentModel.DefaultValue(0)];
        PROPERTY DisabledForeColor AS LONG AUTO

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
        // EVENT HANDLERS
        // ============================================================================

        /// <summary>Custom paint to support Rotation property</summary>
        PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID STRICT
            IF SELF:Rotation != 0
                VAR b := SolidBrush{SELF:ForeColor}
                e:Graphics:TranslateTransform(SELF:Width, SELF:Height / 2)
                e:Graphics:RotateTransform(SELF:Rotation)
                e:Graphics:DrawString(SELF:Text, SELF:Font, b, PointF{0, 0})
            ENDIF
            SUPER:OnPaint(e)
        END METHOD

        // ============================================================================
        // Include text control VFP event wiring
        // ============================================================================
        #include "Headers/TextControlProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
            SELF:BackColor := Color.Transparent
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "Label"
            SELF:_class := "Label"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0

    END CLASS

END NAMESPACE
