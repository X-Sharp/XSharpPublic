// Timer.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.ComponentModel
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// VFP Timer Control - Event-driven timer
    /// Maps VFP Timer properties and methods to WinForms Timer
    ///
    /// Implements: IVFPObject
    ///
    /// Base Class: System.Windows.Forms.Timer
    ///
    /// Note: Preserves Current's VFP-compatible Interval/Enabled auto-enable logic:
    ///       In VFP, setting a non-zero Interval auto-enables the timer on first set.
    ///       Setting Interval to 0 disables the timer.
    /// </summary>
    PARTIAL CLASS Timer INHERIT System.Windows.Forms.Timer IMPLEMENTS IVFPObject

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // PRIVATE FIELDS
        // ============================================================================

        PROTECTED firstSet AS LOGIC
        PRIVATE _baseClass AS STRING
        PRIVATE _class AS STRING
        PRIVATE _classLibrary AS STRING
        PRIVATE _comment AS STRING
        PRIVATE _helpContextID AS LONG
        PRIVATE _whatsThisHelpID AS LONG

        // ============================================================================
        // PROPERTIES
        // ============================================================================

        /// <summary>
        /// Enabled - Override to expose VFP-compatible Enabled property
        /// </summary>
        PUBLIC NEW PROPERTY Enabled AS LOGIC
            GET
                RETURN SUPER:Enabled
            END GET
            SET
                SUPER:Enabled := VALUE
            END SET
        END PROPERTY

        /// <summary>
        /// Interval - Override with VFP-compatible auto-enable behavior:
        ///   Setting Interval to 0 disables the timer.
        ///   Setting Interval to non-zero on first set auto-enables the timer.
        /// </summary>
        PUBLIC NEW PROPERTY Interval AS INT
            GET
                RETURN SUPER:Interval
            END GET
            SET
                IF VALUE == 0
                    SELF:firstSet := SELF:Enabled
                    SELF:Enabled := FALSE
                ELSE
                    SUPER:Interval := VALUE
                    IF SELF:firstSet
                        SELF:Enabled := TRUE
                        SELF:firstSet := FALSE
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        // ============================================================================
        // METHODS
        // ============================================================================

        /// <summary>Start - Enable the timer</summary>
        PUBLIC METHOD Start() AS VOID
            SELF:Enabled := TRUE
        END METHOD

        /// <summary>Stop - Disable the timer</summary>
        PUBLIC METHOD Stop() AS VOID
            SELF:Enabled := FALSE
        END METHOD

        /// <summary>Reset - Stop and restart the timer (resets interval countdown)</summary>
        METHOD Reset() AS VOID STRICT
            SELF:Stop()
            System.Threading.Thread.Sleep(10)
            SELF:Start()
        END METHOD

        /// <summary>Toggle - Toggle between enabled/disabled</summary>
        PUBLIC METHOD Toggle() AS VOID
            SELF:Enabled := !SELF:Enabled
        END METHOD

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR()
            SUPER()
            // In VFP, Timer is enabled by default on first Interval set
            SELF:firstSet := TRUE
            SELF:_baseClass := "Timer"
            SELF:_class := "Timer"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0

    END CLASS

END NAMESPACE
