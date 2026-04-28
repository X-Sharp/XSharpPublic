// CommandButton.prg
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
    /// VFP CommandButton Control - Clickable action button
    /// Maps VFP CommandButton properties and methods to WinForms Button
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPButton
    /// Includes: VFPButton.xh, ButtonControlProperties.xh, FontProperties.xh
    ///
    /// Base Class: System.Windows.Forms.Button
    /// </summary>
    PARTIAL CLASS CommandButton INHERIT System.Windows.Forms.Button IMPLEMENTS IVFPObject, IVFPControl, IVFPButton

        // ============================================================================
        // Include shared VFP button control properties and methods
        // ============================================================================
        #include "Headers/VFPButton.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include button control properties (Caption, Picture, etc.)
        // ============================================================================
        #include "Headers/ButtonControlProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

        // ============================================================================
        // PRIVATE FIELDS
        // ============================================================================

        PRIVATE _vfpStyle AS INT
        PRIVATE _picturePath AS STRING
        PRIVATE _vfpDefault AS LOGIC
        PRIVATE _vfpCancel AS LOGIC
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

        /// <summary>Style - Button style (0=standard, 1=graphical)</summary>
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
        PROPERTY Style AS INT
            GET
                RETURN SELF:_vfpStyle
            END GET
            SET
                SELF:_vfpStyle := VALUE
                IF SELF:_vfpStyle == 1
                    NOP
                ENDIF
            END SET
        END PROPERTY

        /// <summary>Default - This is the default button (activated on Enter)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.Description("Default button")];
        [System.ComponentModel.DefaultValue(FALSE)];
        PROPERTY Default AS LOGIC
            GET
                RETURN SELF:_vfpDefault
            END GET
            SET
                SELF:_vfpDefault := VALUE
                VAR frm := SELF:FindForm()
                IF frm != NULL
                    IF VALUE
                        frm:AcceptButton := SELF
                    ELSEIF frm:AcceptButton == SELF
                        frm:AcceptButton := NULL
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        /// <summary>Cancel - This is the cancel button (activated on Escape)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.Description("Cancel button")];
        [System.ComponentModel.DefaultValue(FALSE)];
        PROPERTY Cancel AS LOGIC
            GET
                RETURN SELF:_vfpCancel
            END GET
            SET
                SELF:_vfpCancel := VALUE
                VAR frm := SELF:FindForm()
                IF frm != NULL
                    IF VALUE
                        frm:CancelButton := SELF
                    ELSEIF frm:CancelButton == SELF
                        frm:CancelButton := NULL
                    ENDIF
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

        /// <summary>Click - Programmatically trigger button click</summary>
        PUBLIC METHOD ClickButton() AS VOID
            SELF:PerformClick()
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
            SELF:BackColor := Color.Transparent
            SELF:_vfpStyle := 0
            SELF:_picturePath := ""
            SELF:_vfpDefault := FALSE
            SELF:_vfpCancel := FALSE
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "CommandButton"
            SELF:_class := "CommandButton"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:Text := "Button"
            SELF:Size := Size{100, 17}

    END CLASS

END NAMESPACE
