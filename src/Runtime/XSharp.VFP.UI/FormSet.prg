// FormSet.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.



USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// VFP-compatible container for a set of related forms.<br/>
/// Holds a collection of <see cref="Form"/> objects exposed via <see cref="Forms"/> (reverse-insertion order,
/// matching VFP generated code) and <see cref="Controls"/> (raw insertion order).<br/>
/// <see cref="Show"/> displays all modeless forms non-blocking then blocks on each modal form in turn.
/// <see cref="Hide"/> hides all forms without closing them.
/// <see cref="Release"/> closes all forms and fires <see cref="vfpDestroy"/>.<br/>
/// <see cref="ActiveForm"/> tracks whichever form most recently received focus via its <c>Activated</c> event.
/// <see cref="SetAll"/> broadcasts a property assignment to all forms and their child controls.
/// </summary>
PARTIAL CLASS FormSet IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

    /// <summary>
    /// Raw insertion-order list of form objects. Used by generated code. Access individual forms via <see cref="Forms"/> (which returns them in the reverse order VFP expects).
    /// </summary>
    PROPERTY Controls AS List<OBJECT> AUTO


    PRIVATE _forms AS List<Form>

    /// <summary>
    /// access individual forms in a form set.
    /// </summary>
    /// <value></value>
    PROPERTY Forms AS List<Form>
        GET
            // First Call ?
            IF SELF:_forms == NULL
                SELF:_forms := List<Form>{ SELF:Controls:Count }
                // Store in Reverse Order, because the generated code is storing in reverse order ! :)
                FOR VAR i:=1 TO SELF:Controls:Count
                    VAR frm := (Form) SELF:Controls[ SELF:Controls:Count - i + 1 ]
                    SELF:_forms:Add(frm)
                    // Wire ActiveForm tracking for forms added by generated code
                    frm:Activated += System.EventHandler{ SELF, @_OnFormActivated() }
                NEXT
            ENDIF
            // Return a copy of the list
            RETURN List<Form>{ SELF:_forms }
        END GET
    END PROPERTY

    /// <summary>
    /// How many Forms in the FormSet
    /// </summary>
    /// <value></value>
    PROPERTY FormCount AS INT GET SELF:Forms:Count

    // -- ControlCount ------------------------------------------------------
    /// <summary>
    /// <c>IVFPOwner</c> requirement. Returns the number of forms in <see cref="Controls"/>. The setter is a no-op â€” the count is derived from the list.
    /// </summary>
    PROPERTY ControlCount AS LONG
        GET ; RETURN SELF:Controls:Count ; END GET
        SET ; NOP ; END SET
    END PROPERTY

    /// <summary>
    /// The <see cref="Form"/> in this set that most recently received focus. Updated automatically by the <c>Activated</c> event handler wired to each form.
    /// </summary>
    PROPERTY ActiveForm AS Form AUTO

    /// <summary>
    /// The current FormSet.
    /// Alias for SELF
    /// </summary>
    /// <value></value>
    PROPERTY ThisFormSet AS OBJECT GET SELF


    PRIVATE _visible AS LOGIC
    /// <summary>
    /// Shows or hides all forms in the set. Setting <c>.T.</c> calls <c>Show()</c> / <c>ShowDialog()</c> on each form according to its <c>WindowType</c>; setting <c>.F.</c> hides all forms.
    /// </summary>
    PROPERTY Visible AS LOGIC
        GET
            RETURN SELF:_visible
        END GET
        SET
            IF ( SELF:Visible != VALUE )
                IF VALUE
                    // Use base WinForms Show for modeless; ShowDialog for modal
                    FOREACH frm AS Form IN SELF:Forms
                        IF frm:MDIForm .OR. frm:WindowType == 0
                            ((System.Windows.Forms.Form) frm):Show()
                        ELSE
                            frm:ShowDialog()
                        ENDIF
                    NEXT
                ELSE
                    FOREACH frm AS Form IN SELF:Forms
                        frm:Visible := FALSE
                    NEXT
                ENDIF
            ENDIF
            SELF:_visible := VALUE
        END SET
    END PROPERTY

    /// <summary>
    /// Add a Form to this FormSet at runtime. Wires ActiveForm tracking
    /// and invalidates the Forms cache.
    /// </summary>
    METHOD AddForm(oForm AS Form) AS VOID
        SELF:Controls:Add(oForm)
        SELF:_forms := NULL          // invalidate cache
        // Wire focus ? ActiveForm tracking
        oForm:Activated += System.EventHandler{ SELF, @_OnFormActivated() }

    /// <summary>
    /// Remove a Form from this FormSet at runtime.
    /// </summary>
    METHOD RemoveForm(oForm AS Form) AS VOID
        SELF:Controls:Remove(oForm)
        SELF:_forms := NULL          // invalidate cache
        oForm:Activated -= System.EventHandler{ SELF, @_OnFormActivated() }

    /// <summary>
    /// Handles the <c>Activated</c> event on any form in the set; updates <see cref="ActiveForm"/> to the focused form.
    /// </summary>
    PRIVATE METHOD _OnFormActivated(sender AS OBJECT, e AS System.EventArgs) AS VOID
        IF sender IS Form VAR frm
            SELF:ActiveForm := frm
        ENDIF

    CONSTRUCTOR()
        // The XPorter is the same for FormSet and Form with controls, so the generated code will use Controls
        SELF:Controls := List<OBJECT>{ }
        SELF:_visible := FALSE
        SELF:OnVFPInit()
        RETURN

    /// <summary>
    /// Show all forms in the set. Respects each form's WindowType (modal/modeless).
    /// Modeless forms (WindowType=0) are shown non-blocking first; modal forms
    /// (WindowType=1) call ShowDialog and block ï¿½ matching VFP FormSet behaviour.
    /// </summary>
    METHOD Show() AS VOID STRICT
        // Pass 1: show all modeless forms without blocking
        FOREACH frm AS Form IN SELF:Forms
            IF frm:MDIForm .OR. frm:WindowType == 0
                // Call the base WinForms Show directly to avoid the VFP modal override
                ((System.Windows.Forms.Form) frm):Show()
            ENDIF
        NEXT
        // Pass 2: show modal forms (each will block until closed)
        FOREACH frm AS Form IN SELF:Forms
            IF !frm:MDIForm .AND. frm:WindowType != 0
                frm:ShowDialog()
            ENDIF
        NEXT
        SELF:_visible := TRUE
        SELF:OnVFPActivate()

    /// <summary>
    /// Hide all forms in the set without closing them.
    /// </summary>
    METHOD Hide() AS VOID STRICT
        FOREACH frm AS Form IN SELF:Forms
            frm:Visible := FALSE
        NEXT
        SELF:_visible := FALSE
        SELF:OnVFPDeactivate()

    /// <summary>
    /// Refresh all forms in the set (re-reads data bindings and redraws).
    /// </summary>
    METHOD Refresh() AS VOID STRICT
        FOREACH frm AS Form IN SELF:Forms
            frm:Refresh()
        NEXT

    /// <summary>
    /// Release (close) all forms in the set and clear the form list.
    /// Mirrors VFP RELEASE THISFORMSET behaviour.
    /// </summary>
    METHOD Release() AS USUAL CLIPPER
        FOREACH frm AS Form IN SELF:Forms
            frm:Activated -= System.EventHandler{ SELF, @_OnFormActivated() }
            frm:Release()
        NEXT
        SELF:_forms := NULL          // invalidate cache
        SELF:Controls:Clear()
        SELF:_visible := FALSE
        SELF:ActiveForm := NULL_OBJECT
        SELF:OnVFPDestroy()
        RETURN NIL

    // -- SetAll -----------------------------------------------------------
    /// <summary>
    /// Broadcast a property assignment to all forms (and optionally their
    /// child controls) in the FormSet, matching VFP SetAll() semantics.
    /// SetAll(cProperty, uValue [, cClass])
    /// </summary>
    METHOD SetAll(cProperty, uValue, cClass) AS USUAL CLIPPER
        IF !IsString(cProperty)
            RETURN NIL
        ENDIF
        LOCAL cProp := (STRING) cProperty AS STRING
        LOCAL cFilter AS STRING
        cFilter := IIF(IsString(cClass), (STRING)cClass, "")
        FOREACH frm AS Form IN SELF:Forms
            IF String.IsNullOrEmpty(cFilter) .OR. String.Compare(frm:Class, cFilter, TRUE) == 0
                TRY
                    IVarPut(frm, cProp, uValue)
                CATCH
                    NOP
                END TRY
            ENDIF
            // Also broadcast to child controls of each form
            FOREACH ctrl AS System.Windows.Forms.Control IN frm:Controls
                IF String.IsNullOrEmpty(cFilter) .OR. String.Compare(ctrl:GetType():Name, cFilter, TRUE) == 0
                    TRY
                        IVarPut(ctrl, cProp, uValue)
                    CATCH
                        NOP
                    END TRY
                ENDIF
            NEXT
        NEXT
        RETURN NIL

    // -- Lifecycle Events ------------------------------------------------

    PRIVATE _VFPInit AS VFPOverride
    /// <summary>
    /// Name of the VFP method called when the FormSet is first created. Fired in the constructor.
    /// </summary>
    [Category("VFP Events"), Description("Occurs when the FormSet is created.")];
    [DefaultValue(NULL)];
    PROPERTY vfpInit AS STRING GET _VFPInit?:SendTo SET SELF:_VFPInit := VFPOverride{SELF, VALUE}

    /// <summary>
    /// Dispatches the <see cref="vfpInit"/> handler.
    /// </summary>
    PRIVATE METHOD OnVFPInit() AS VOID
        IF SELF:_VFPInit != NULL
            SELF:_VFPInit:Call()
        ENDIF

    PRIVATE _VFPDestroy AS VFPOverride
    /// <summary>
    /// Name of the VFP method called when the FormSet is released. Fired by <see cref="Release"/>.
    /// </summary>
    [Category("VFP Events"), Description("Occurs when the FormSet is released.")];
    [DefaultValue(NULL)];
    PROPERTY vfpDestroy AS STRING GET _VFPDestroy?:SendTo SET SELF:_VFPDestroy := VFPOverride{SELF, VALUE}

    /// <summary>
    /// Dispatches the <see cref="vfpDestroy"/> handler.
    /// </summary>
    PRIVATE METHOD OnVFPDestroy() AS VOID
        IF SELF:_VFPDestroy != NULL
            SELF:_VFPDestroy:Call()
        ENDIF

    PRIVATE _VFPActivate AS VFPOverride
    /// <summary>
    /// Name of the VFP method called when the FormSet is shown. Fired by <see cref="Show"/>.
    /// </summary>
    [Category("VFP Events"), Description("Occurs when the FormSet is shown (activated).")];
    [DefaultValue(NULL)];
    PROPERTY vfpActivate AS STRING GET _VFPActivate?:SendTo SET SELF:_VFPActivate := VFPOverride{SELF, VALUE}

    /// <summary>
    /// Dispatches the <see cref="vfpActivate"/> handler.
    /// </summary>
    PRIVATE METHOD OnVFPActivate() AS VOID
        IF SELF:_VFPActivate != NULL
            SELF:_VFPActivate:Call()
        ENDIF

    PRIVATE _VFPDeactivate AS VFPOverride
    /// <summary>
    /// Name of the VFP method called when the FormSet is hidden. Fired by <see cref="Hide"/>.
    /// </summary>
    [Category("VFP Events"), Description("Occurs when the FormSet is hidden (deactivated).")];
    [DefaultValue(NULL)];
    PROPERTY vfpDeactivate AS STRING GET _VFPDeactivate?:SendTo SET SELF:_VFPDeactivate := VFPOverride{SELF, VALUE}

    /// <summary>
    /// Dispatches the <see cref="vfpDeactivate"/> handler.
    /// </summary>
    PRIVATE METHOD OnVFPDeactivate() AS VOID
        IF SELF:_VFPDeactivate != NULL
            SELF:_VFPDeactivate:Call()
        ENDIF

END CLASS
END NAMESPACE // XSharp.VFP.UI
