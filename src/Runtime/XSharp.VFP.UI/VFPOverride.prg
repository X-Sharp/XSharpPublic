// VFPOverride.prg

USING System
USING System.Collections.Generic
USING System.Text
USING  System.Reflection

BEGIN NAMESPACE XSharp.VFP.UI
    /// <summary>
    /// The VFPOverride class.
    /// At code generation it captures the Owner and the Late-bound method to be called.
    /// When the Call() is called, a late-bound called is done, using Send()
    /// </summary>
    CLASS VFPOverride
        PRIVATE _owner AS OBJECT
        PRIVATE _sendTo AS STRING

        PROPERTY InCall AS LOGIC AUTO GET PRIVATE SET

        PROPERTY SendTo AS STRING GET _sendTo

        CONSTRUCTOR( o AS OBJECT, s AS STRING )
            // Before calling the method we will check we are at Form Level
            // But at creation-time, the Control may hasn't been Added to Controls...so no parent.
            _owner := o
            _sendTo := s
            SELF:InCall := FALSE
            IF _sendTo != NULL
                IF _sendTo:EndsWith("()")
                    _sendTo := _sendTo:Substring(0, _sendTo:Length-2 )
                ENDIF
            ENDIF

        METHOD Call( args AS USUAL[] ) AS USUAL
            //
            TRY
                SELF:InCall := TRUE
                IF _owner != NULL .AND. _sendTo != NULL
                    IF IsMethod( _owner, _sendTo )
                        RETURN Send( _owner, _sendTo, args )
                    ELSE
                        CallParent( args )
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

        METHOD Call( ) AS USUAL
            //
            TRY
                SELF:InCall := TRUE
                IF _owner != NULL .AND. _sendTo != NULL
                    IF IsMethod( _owner, _sendTo )
                        RETURN Send( _owner, _sendTo )
                    ELSE
                        CallParent( NULL )
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
                    UNTIL (oForm IS System.Windows.Forms.Form)
                    //
                    IF IsMethod( oForm, _sendTo )
                        IF args != NULL
                            Send( oForm, _sendTo, args )
                        ELSE
                            Send( oForm, _sendTo )
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF


    END CLASS
END NAMESPACE
