// Container.generated.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.Drawing

BEGIN NAMESPACE XSharp.VFP.UI

    PARTIAL CLASS Container

        // ActiveControl: the child control that currently has focus
        PROPERTY ActiveControl AS USUAL
            GET
                RETURN (USUAL) SUPER:ActiveControl
            END GET
        END PROPERTY

        // BorderStyle: VFP 0=no border, 1=single-line border
        NEW PROPERTY BorderStyle AS LONG
            GET
                IF SUPER:BorderStyle == System.Windows.Forms.BorderStyle.FixedSingle
                    RETURN 1
                ENDIF
                RETURN 0
            END GET
            SET
                IF VALUE == 1
                    SUPER:BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle
                ELSE
                    SUPER:BorderStyle := System.Windows.Forms.BorderStyle.None
                ENDIF
            END SET
        END PROPERTY

        PROPERTY BorderWidth AS LONG AUTO
        PROPERTY ColorSource AS LONG AUTO
        PROPERTY SpecialEffect AS LONG AUTO

        // ControlCount: live count of child controls
        NEW PROPERTY ControlCount AS LONG
            GET ; RETURN SELF:Controls:Count ; END GET
        END PROPERTY

        // Picture: sets the container background image
        PRIVATE _picture AS STRING
        PROPERTY Picture AS STRING
            GET
                IF SELF:_picture == NULL ; SELF:_picture := "" ; ENDIF
                RETURN _picture
            END GET
            SET
                _picture := VALUE
                SELF:BackgroundImage := VFPTools.ImageFromFile(VALUE)
            END SET
        END PROPERTY

        // ── Resize event ─────────────────────────────────────────────────────

        PRIVATE _VFPResize AS VFPOverride
        [System.ComponentModel.Category("VFP Events"),System.ComponentModel.DefaultValue("")];
        PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET Set_Resize( VFPOverride{SELF, VALUE} )

        METHOD Set_Resize( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPResize := methodCall

        PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
            SUPER:OnResize(e)
            SELF:Resize()

        VIRTUAL METHOD Resize() AS VOID
            IF SELF:_VFPResize != NULL
                SELF:_VFPResize:Call()
            ENDIF

        // ── Moved event ──────────────────────────────────────────────────────

        PRIVATE _VFPMoved AS VFPOverride
        [System.ComponentModel.Category("VFP Events"),System.ComponentModel.DefaultValue("")];
        PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET Set_Moved( VFPOverride{SELF, VALUE} )

        METHOD Set_Moved( methodCall AS VFPOverride ) AS VOID
            SELF:_VFPMoved := methodCall

        PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
            SUPER:OnMove(e)
            SELF:Moved()

        VIRTUAL METHOD Moved() AS VOID
            IF SELF:_VFPMoved != NULL
                SELF:_VFPMoved:Call()
            ENDIF

        // C-4: Refresh moved to Container.prg (calls _VFPRefresh + iterates children)

    END CLASS
END NAMESPACE // XSharp.VFP.UI
