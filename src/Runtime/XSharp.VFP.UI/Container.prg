// Container.prg
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
    /// VFP Container Control - Grouping and layout container
    /// Maps VFP Container properties and methods to WinForms UserControl
    ///
    /// Implements: IDynamicProperties, IDynamicProperties2, IVFPOwner
    /// Includes: VFPProperties.xh (XSharp dynamic properties support)
    ///           ControlProperties.xh (VFP control event wiring)
    ///           Tooltips.xh (tooltip support)
    ///
    /// Base Class: System.Windows.Forms.UserControl
    /// </summary>
    PARTIAL CLASS Container INHERIT System.Windows.Forms.UserControl IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

        // ============================================================================
        // Include XSharp dynamic properties support
        // ============================================================================
        #include "XSharp/VFPProperties.xh"

        // ============================================================================
        // Include common VFP control properties and event wiring
        // ============================================================================
        #include "Headers/ControlProperties.xh"

        // ============================================================================
        // Include tooltip support
        // ============================================================================
        #include "Headers/Tooltips.xh"

        // ============================================================================
        // PROPERTIES - Container-specific VFP API
        // ============================================================================

        PROPERTY BorderColor AS System.Drawing.Color AUTO

        PRIVATE _backStyle := 1 AS INT
        [System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
        PROPERTY BackStyle AS INT
            GET
                RETURN _backStyle
            END GET
            SET
                _backStyle := VALUE
                IF VALUE == 0
                    SELF:BackColor := System.Drawing.Color.Transparent
                ENDIF
            END SET
        END PROPERTY

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
            SELF:BackColor := Color.Transparent
            SELF:Size := Size{75, 75}
            RETURN

    END CLASS

END NAMESPACE // XSharp.VFP.UI
