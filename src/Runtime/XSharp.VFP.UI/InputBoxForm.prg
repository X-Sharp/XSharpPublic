//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Drawing
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// Modal dialog that supports the VFP function INPUTBOX(): prompt + text box,
    /// OK/Cancel buttons and an optional timeout.
    /// </summary>
    INTERNAL CLASS InputBoxForm INHERIT System.Windows.Forms.Form
        PRIVATE _txtInput     AS System.Windows.Forms.TextBox
        PRIVATE _timer        AS System.Windows.Forms.Timer
        PRIVATE _timedOut     := FALSE AS LOGIC
        PRIVATE _timeoutValue AS STRING
        PRIVATE _cancelValue  AS STRING

        PUBLIC CONSTRUCTOR(cPrompt AS STRING, cCaption AS STRING, cDefault AS STRING, ;
                           nTimeout AS LONG, cTimeoutValue AS STRING, cCancelValue AS STRING) STRICT
            SUPER()
            SELF:_timeoutValue := cTimeoutValue
            SELF:_cancelValue  := cCancelValue
            SELF:BuildLayout(cPrompt, cCaption, cDefault)

            IF nTimeout > 0
                SELF:_timer          := System.Windows.Forms.Timer{}
                SELF:_timer:Interval := nTimeout
                SELF:_timer:Tick     += System.EventHandler{ SELF, @OnTimerTick() }
                SELF:_timer:Start()
            ENDIF
            RETURN

        PRIVATE METHOD BuildLayout(cPrompt AS STRING, cCaption AS STRING, cDefault AS STRING) AS VOID STRICT
            SELF:SuspendLayout()

            VAR oLabel := System.Windows.Forms.Label{}
            oLabel:AutoSize := FALSE
            oLabel:Location := System.Drawing.Point{12, 14}
            oLabel:Size     := System.Drawing.Size{410, 20}
            oLabel:Text     := cPrompt

            SELF:_txtInput          := System.Windows.Forms.TextBox{}
            SELF:_txtInput:Location := System.Drawing.Point{12, 36}
            SELF:_txtInput:Size     := System.Drawing.Size{410, 23}
            SELF:_txtInput:Text     := cDefault

            VAR oOk := System.Windows.Forms.Button{}
            oOk:Text         := "OK"
            oOk:DialogResult := System.Windows.Forms.DialogResult.OK
            oOk:Location     := System.Drawing.Point{266, 72}
            oOk:Size         := System.Drawing.Size{75, 25}

            VAR oCancel := System.Windows.Forms.Button{}
            oCancel:Text         := "Cancel"
            oCancel:DialogResult := System.Windows.Forms.DialogResult.Cancel
            oCancel:Location     := System.Drawing.Point{347, 72}
            oCancel:Size         := System.Drawing.Size{75, 25}

            SELF:Controls:Add(oLabel)
            SELF:Controls:Add(SELF:_txtInput)
            SELF:Controls:Add(oOk)
            SELF:Controls:Add(oCancel)

            SELF:AcceptButton    := oOk
            SELF:CancelButton    := oCancel
            SELF:Text            := cCaption
            SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog
            SELF:StartPosition   := System.Windows.Forms.FormStartPosition.CenterScreen
            SELF:MaximizeBox     := FALSE
            SELF:MinimizeBox     := FALSE
            SELF:ShowIcon        := FALSE
            SELF:ShowInTaskbar   := FALSE
            SELF:ClientSize      := System.Drawing.Size{434, 110}

            SELF:ResumeLayout(FALSE)
            SELF:_txtInput:SelectAll()
            RETURN

        PRIVATE METHOD OnTimerTick(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:_timer:Stop()
            SELF:_timedOut := TRUE
            SELF:Close()
            RETURN

        PUBLIC METHOD GetResult() AS STRING STRICT
            IF SELF:_timedOut
                RETURN SELF:_timeoutValue
            ELSEIF SELF:DialogResult == System.Windows.Forms.DialogResult.OK
                RETURN SELF:_txtInput:Text
            ENDIF
            RETURN SELF:_cancelValue

        PROTECTED OVERRIDE METHOD Dispose(disposing AS LOGIC) AS VOID STRICT
            IF disposing .AND. SELF:_timer != NULL
                SELF:_timer:Dispose()
            ENDIF
            SUPER:Dispose(disposing)
            RETURN

    END CLASS

END NAMESPACE
