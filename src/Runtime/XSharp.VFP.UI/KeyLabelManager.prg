// KeyLabelManager.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Runtime support for VFP  ON KEY LABEL <KeyLabel> [<Command>]
// Maps VFP key-label strings to .NET Keys values, registers handlers
// via a global WinForms IMessageFilter, and provides the KEYBOARD helper.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING XSharp.VFP.UI

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>Static store for registered ON KEY LABEL handlers.</summary>
    STATIC CLASS __VFPKeyLabelState

        /// <summary>Map of Keys value → codeblock handler (NIL = registered but cleared).</summary>
        STATIC PROPERTY Handlers AS Dictionary<Keys, USUAL> AUTO

        /// <summary>Keeps the filter alive so GC does not collect it.</summary>
        STATIC PROPERTY Filter AS __VFPKeyFilter AUTO

        STATIC PROPERTY FilterInstalled AS LOGIC AUTO

        STATIC CONSTRUCTOR()
            Handlers        := Dictionary<Keys, USUAL>{}
            Filter          := NULL
            FilterInstalled := FALSE

    END CLASS

    /// <summary>
    /// WinForms message filter that intercepts WM_KEYDOWN and dispatches
    /// any registered ON KEY LABEL handler, suppressing the original keystroke.
    /// </summary>
    CLASS __VFPKeyFilter IMPLEMENTS IMessageFilter

        PRIVATE CONST WM_KEYDOWN := 0x0100 AS INT

        METHOD PreFilterMessage(msg REF Message) AS LOGIC
            IF msg:Msg != WM_KEYDOWN
                RETURN FALSE
            ENDIF
            // Build the Keys value including modifier bits
            LOCAL key AS Keys
            key := (Keys) msg:WParam:ToInt32()
            LOCAL mods := Control.ModifierKeys AS Keys
            IF (mods & Keys.Alt)     != 0 ; key |= Keys.Alt     ; ENDIF
            IF (mods & Keys.Control) != 0 ; key |= Keys.Control ; ENDIF
            IF (mods & Keys.Shift)   != 0 ; key |= Keys.Shift   ; ENDIF
            // Look up and invoke handler
            LOCAL handler AS USUAL
            IF __VFPKeyLabelState.Handlers:TryGetValue(key, OUT handler)
                IF handler != NIL
                    Eval(handler)
                    RETURN TRUE   // suppress the original keystroke
                ENDIF
            ENDIF
            RETURN FALSE

        END METHOD

    END CLASS

END NAMESPACE

// ── Public API ────────────────────────────────────────────────────────────────

/// <summary>
/// Implements ON KEY LABEL &lt;label&gt; [handler].
/// Pass NIL as block to clear the handler (ON KEY LABEL &lt;label&gt; with no command).
/// </summary>
FUNCTION __VFPOnKeyLabel(cLabel AS STRING, block AS USUAL) AS VOID
    LOCAL key AS Keys
    key := __VFPLabelToKeys(cLabel)
    IF key == Keys.None
        RETURN
    ENDIF
    IF block == NIL
        __VFPKeyLabelState.Handlers:Remove(key)
    ELSE
        __VFPKeyLabelState.Handlers[key] := block
        IF !__VFPKeyLabelState.FilterInstalled
            __VFPKeyLabelState.Filter          := __VFPKeyFilter{}
            System.Windows.Forms.Application.AddMessageFilter(__VFPKeyLabelState.Filter)
            __VFPKeyLabelState.FilterInstalled := TRUE
        ENDIF
    ENDIF

/// <summary>
/// Implements KEYBOARD &lt;cStr&gt;: sends cStr into the WinForms message queue.
/// Control characters (CHR(13) = Enter, CHR(27) = Esc, etc.) are converted
/// to the corresponding SendKeys escape sequences.
/// </summary>
FUNCTION __VFPKeyboard(cStr AS STRING) AS VOID
    IF String.IsNullOrEmpty(cStr)
        RETURN
    ENDIF
    LOCAL sb AS StringBuilder
    sb := StringBuilder{}
    FOREACH VAR ch IN cStr
        LOCAL n := (INT) ch AS INT
        IF n == 13
            sb:Append("{ENTER}")
        ELSEIF n == 27
            sb:Append("{ESC}")
        ELSEIF n == 9
            sb:Append("{TAB}")
        ELSEIF n == 8
            sb:Append("{BACKSPACE}")
        ELSEIF n == 127
            sb:Append("{DELETE}")
        ELSEIF ch == c'{'
            sb:Append("{{}}")
        ELSEIF ch == c'}'
            sb:Append("{}}")
        ELSEIF ch == c'+'
            sb:Append("{+}")
        ELSEIF ch == c'^'
            sb:Append("{^}")
        ELSEIF ch == c'%'
            sb:Append("{%}")
        ELSEIF ch == c'~'
            sb:Append("{~}")
        ELSEIF ch == c'('
            sb:Append("{(}")
        ELSEIF ch == c')'
            sb:Append("{)}")
        ELSEIF ch == c'['
            sb:Append("{[}")
        ELSEIF ch == c']'
            sb:Append("{]}")
        ELSEIF n >= 32
            sb:Append(ch)
        ENDIF
    NEXT
    IF sb:Length > 0
        SendKeys.Send(sb:ToString())
    ENDIF

/// <summary>
/// Converts a VFP key-label string (e.g. "ESC", "ALT+C", "CTRL+F4") to a
/// .NET Keys value.  Returns Keys.None for unrecognised labels.
/// </summary>
FUNCTION __VFPLabelToKeys(cLabel AS STRING) AS Keys
    LOCAL upper := cLabel:Trim():ToUpperInvariant() AS STRING
    // Strip modifier prefixes
    LOCAL mods := Keys.None AS Keys
    LOCAL changed := TRUE AS LOGIC
    DO WHILE changed
        changed := FALSE
        IF upper:StartsWith("ALT+")
            mods   |= Keys.Alt    ; upper := upper:Substring(4) ; changed := TRUE
        ELSEIF upper:StartsWith("CTRL+")
            mods   |= Keys.Control ; upper := upper:Substring(5) ; changed := TRUE
        ELSEIF upper:StartsWith("SHIFT+")
            mods   |= Keys.Shift   ; upper := upper:Substring(6) ; changed := TRUE
        ENDIF
    ENDDO
    // Named keys
    LOCAL base AS Keys
    IF upper == "ESC"        ; base := Keys.Escape
    ELSEIF upper == "ENTER"      .OR. upper == "RETURN" ; base := Keys.Return
    ELSEIF upper == "TAB"        ; base := Keys.Tab
    ELSEIF upper == "BACKSPACE"  ; base := Keys.Back
    ELSEIF upper == "DEL"        .OR. upper == "DELETE" ; base := Keys.Delete
    ELSEIF upper == "INS"        .OR. upper == "INSERT" ; base := Keys.Insert
    ELSEIF upper == "HOME"       ; base := Keys.Home
    ELSEIF upper == "END"        ; base := Keys.End
    ELSEIF upper == "PGUP"       ; base := Keys.PageUp
    ELSEIF upper == "PGDN"       ; base := Keys.PageDown
    ELSEIF upper == "UPARROW"    ; base := Keys.Up
    ELSEIF upper == "DOWNARROW"  ; base := Keys.Down
    ELSEIF upper == "LEFTARROW"  ; base := Keys.Left
    ELSEIF upper == "RIGHTARROW" ; base := Keys.Right
    ELSEIF upper == "SPACE"      ; base := Keys.Space
    ELSEIF upper == "F1"  ; base := Keys.F1
    ELSEIF upper == "F2"  ; base := Keys.F2
    ELSEIF upper == "F3"  ; base := Keys.F3
    ELSEIF upper == "F4"  ; base := Keys.F4
    ELSEIF upper == "F5"  ; base := Keys.F5
    ELSEIF upper == "F6"  ; base := Keys.F6
    ELSEIF upper == "F7"  ; base := Keys.F7
    ELSEIF upper == "F8"  ; base := Keys.F8
    ELSEIF upper == "F9"  ; base := Keys.F9
    ELSEIF upper == "F10" ; base := Keys.F10
    ELSEIF upper == "F11" ; base := Keys.F11
    ELSEIF upper == "F12" ; base := Keys.F12
    ELSEIF upper:Length == 1
        LOCAL c := upper[0] AS CHAR
        IF c >= c'A' .AND. c <= c'Z'
            base := (Keys) ((INT) Keys.A + ((INT) c - (INT) c'A'))
        ELSEIF c >= c'0' .AND. c <= c'9'
            base := (Keys) ((INT) Keys.D0 + ((INT) c - (INT) c'0'))
        ELSE
            RETURN Keys.None
        ENDIF
    ELSE
        RETURN Keys.None
    ENDIF
    RETURN mods | base

