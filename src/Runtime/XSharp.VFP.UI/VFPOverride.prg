// VFPOverride.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING  System.Reflection

BEGIN NAMESPACE XSharp.VFP.UI
/// <summary>
/// Stores a VFP event handler as a late-bound method reference and dispatches it at runtime.<br/>
/// Each VFP event property (e.g. <c>vfpClick</c>) creates a <c>VFPOverride</c> that captures the
/// <paramref name="o">owner object</paramref> and the method name string at assignment time.
/// When the underlying WinForms event fires, <see cref="Call()"/> or <see cref="Call(USUAL[])"/>
/// uses <c>Send()</c> to invoke the method on the owner.<br/>
/// If the method is not found directly on the owner, <see cref="CallParent"/> walks up the
/// <c>Controls</c> parent chain until it reaches a <c>Form</c>, looking for the method there —
/// this implements VFP's scope-resolution behaviour where event handlers can be defined on the
/// enclosing form rather than on the control itself.<br/>
/// <see cref="InCall"/> is set to <c>.T.</c> for the duration of the call, allowing re-entrancy guards.
/// Trailing <c>"()"</c> is stripped from the method name to tolerate VFP-style notation.
/// </summary>
CLASS VFPOverride
    PRIVATE _owner AS OBJECT
    PRIVATE _sendTo AS STRING

    /// <summary>
    /// <c>.T.</c> while a <see cref="Call()"/> dispatch is in progress; <c>.F.</c> otherwise.
    /// </summary>
    PROPERTY InCall AS LOGIC AUTO GET PRIVATE SET

    /// <summary>
    /// The method name this instance will dispatch to.
    /// </summary>
    PROPERTY SendTo AS STRING GET _sendTo

    /// <summary>
    /// Creates a new <c>VFPOverride</c> bound to <paramref name="o"/> and the method named <paramref name="s"/>.<br/>
    /// A trailing <c>"()"</c> suffix on <paramref name="s"/> is removed automatically.
    /// </summary>
    CONSTRUCTOR( o AS OBJECT, s AS STRING )
        _owner := o
        _sendTo := s
        SELF:InCall := FALSE
        IF _sendTo != NULL
            IF _sendTo:EndsWith("()")
                _sendTo := _sendTo:Substring(0, _sendTo:Length-2 )
            ENDIF
        ENDIF

    /// <summary>
    /// Dispatches the method with the supplied argument array. Walks up the parent chain if the method is not found on the owner.
    /// </summary>
    METHOD Call( args AS USUAL[] ) AS USUAL
        //
        TRY
            SELF:InCall := TRUE
            IF _owner != NULL .AND. _sendTo != NULL
                IF IsMethod( _owner, _sendTo )
                    RETURN Send( _owner, _sendTo, args )
                ELSE
                    SELF:CallParent( args )
                ENDIF
            ELSE
                RETURN NULL
            ENDIF
        CATCH ex AS Exception
            THROW ex
        FINALLY
            SELF:InCall := FALSE
        END TRY
        RETURN NULL


    /// <summary>
    /// Dispatches the method with no arguments. Walks up the parent chain if the method is not found on the owner.
    /// </summary>
    METHOD Call( ) AS USUAL
        //
        TRY
            SELF:InCall := TRUE
            IF _owner != NULL .AND. _sendTo != NULL
                IF IsMethod( _owner, _sendTo )
                    RETURN Send( _owner, _sendTo )
                ELSE
                    SELF:CallParent( NULL )
                ENDIF
            ELSE
                RETURN NULL
            ENDIF
        CATCH ex AS Exception
            THROW ex
        FINALLY
            SELF:InCall := FALSE
        END TRY
        RETURN NULL

    /// <summary>
    /// Walks the <c>Controls.Parent</c> chain from the owner up to (but not including) the <c>Form</c>, invoking the method on the first ancestor that has it.
    /// </summary>
    PRIVATE METHOD CallParent( args AS USUAL[] ) AS VOID
        LOCAL oForm AS OBJECT
        //
        oForm := _owner
        // Send call to the Form
        IF !(oForm IS System.Windows.Forms.Form)
            // At least a Control ??
            IF oForm IS System.Windows.Forms.Control
                // Search the Form in the Parent tree
                REPEAT
                    LOCAL ctrl := oForm AS System.Windows.Forms.Control
                    oForm := ctrl:Parent
                    //
                    IF IsMethod( oForm, _sendTo )
                        IF args != NULL
                            Send( oForm, _sendTo, args )
                        ELSE
                            Send( oForm, _sendTo )
                        ENDIF
                        EXIT
                    ENDIF
                UNTIL (oForm IS System.Windows.Forms.Form)

            ENDIF
        ENDIF


END CLASS
END NAMESPACE
