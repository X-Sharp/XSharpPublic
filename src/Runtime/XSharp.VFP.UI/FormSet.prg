// FormSet.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP FormSet - Container for multiple Form objects
    /// Manages a collection of Forms with Visible delegation
    ///
    /// Implements: IDynamicProperties, IDynamicProperties2, IVFPOwner
    /// </summary>
    PARTIAL CLASS FormSet IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

        /// <summary>
        /// Same as the Forms property.
        /// The XPorter-generated code stores forms here in Controls.
        /// </summary>
        PROPERTY Controls AS List<OBJECT> AUTO

        PRIVATE _forms AS List<Form>

        /// <summary>
        /// Access individual forms in a form set.
        /// Returns forms in the order they were added (stored in reverse by generated code).
        /// </summary>
        PROPERTY Forms AS List<Form>
            GET
                // First Call ?
                IF SELF:_forms == NULL
                    SELF:_forms := List<Form>{SELF:Controls:Count}
                    // Store in Reverse Order, because the generated code stores in reverse order
                    FOR VAR i := 1 TO SELF:Controls:Count
                        SELF:_forms:Add((Form)SELF:Controls[SELF:Controls:Count - i + 1])
                    NEXT
                ENDIF
                // Return a copy of the list
                RETURN List<Form>{SELF:_forms}
            END GET
        END PROPERTY

        /// <summary>
        /// How many Forms in the FormSet
        /// </summary>
        PROPERTY FormCount AS INT GET SELF:Forms:Count

        PROPERTY ActiveForm AS Form AUTO

        /// <summary>
        /// The current FormSet.
        /// Alias for SELF
        /// </summary>
        PROPERTY ThisFormSet AS OBJECT GET SELF

        PRIVATE _visible AS LOGIC
        PROPERTY Visible AS LOGIC
            GET
                RETURN SELF:_visible
            END GET
            SET
                IF SELF:Visible != VALUE
                    FOREACH frm AS Form IN SELF:Forms
                        IF VALUE
                            frm:Show()
                        ELSE
                            frm:Visible := FALSE
                        ENDIF
                    NEXT
                ENDIF
                SELF:_visible := VALUE
            END SET
        END PROPERTY

        CONSTRUCTOR()
            // The XPorter uses the same pattern for FormSet and Form with controls,
            // so the generated code uses Controls
            SELF:Controls := List<OBJECT>{}
            SELF:_visible := FALSE
            RETURN

    END CLASS

END NAMESPACE // XSharp.VFP.UI
