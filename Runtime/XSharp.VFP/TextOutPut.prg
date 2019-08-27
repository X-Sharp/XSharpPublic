//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#command GETSTATE <type> <SET>	      =>				 ;
		RETURN XSharp.RuntimeState.GetValue\< <type> \>( <SET> )

#command SETSTATE <type> <SET>	<VALUE> =>				 ;
		RETURN XSharp.RuntimeState.SetValue\< <type> \>( <SET> , <VALUE> )			


/// <summary>
/// Retrieve the setting that determines if TextMerge is active
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>

Function SetTextMerge() AS LOGIC
   GETSTATE LOGIC Set.TextMerge

Function SetTextMerge(lSet as LOGIC) AS LOGIC
   SETSTATE LOGIC Set.TextMerge lSet


FUNCTION SetTextMerge(cFile as STRING) AS LOGIC
    local hFile as IntPtr
    IF _TEXT != -1
        hFile := IntPtr{_TEXT}
        FClose(hFile)
        _TEXT := -1
    ENDIF
    IF !String.IsNullOrEmpty(cFile)
        hFile := FCreate(cFile)
        IF FError() == 0
            _TEXT := hFile:ToInt64()
        ENDIF
    ENDIF
    RETURN _TEXT != -1

Function __TextOut(cText as STRING, lNewLine as LOGIC) AS VOID
    local hFile as IntPtr
    if _TEXT != -1
        hFile := IntPtr{ _TEXT}
        if (lNewLine)
            FWrite(hFile, System.Environment.NewLine)
            IF Slen(_PRETEXT) > 0
                FWrite(hFile, _PRETEXT)
            ENDIF
        ENDIF
        FWrite(hFile, cText)
    ENDIF
    RETURN

Function __TextSupport(cText as STRING, lNoShow as LOGIC, nFlags as LONG, uPreText as USUAL) AS STRING
    local cPreText as STRING
    LOCAL nPreText as LONG
    LOCAL lDeleteSpaces := FALSE as LOGIC
    LOCAL lDeleteTabs   := FALSE AS LOGIC
    LOCAL lDeleteCR     := FALSE AS LOGIC
    LOCAL lDeleteLF     := FALSE AS LOGIC
    IF IsString(uPreText)
        cPreText := uPreText
    ELSE
        cPreText := _PRETEXT
    ENDIF
    IF IsNumeric(uPreText)
        nPreText := uPreText
        IF nPreText < 0 .or. nPreText > 15
            // Throw an exception
        ELSE
            lDeleteSpaces   := _AND(nPreText, 1) == 1
            lDeleteTabs     := _AND(nPreText, 2) == 2
            lDeleteCR       := _AND(nPreText, 4) == 4
            lDeleteLF       := _AND(nPreText, 8) == 8
        ENDIF
    ENDIF
    var result := System.Text.StringBuilder{cText:Length}
    local lStartOfLine as LOGIC
    lStartOfLine := TRUE
    result:Append(cPreText)
    FOREACH VAR cChar in cText
        SWITCH cChar
        CASE c' '
            IF lStartOfLine .and. lDeleteSpaces
                NOP
            ELSE
                result:Append(cChar)
            ENDIF
        CASE c'\t'
            IF lStartOfLine .and. lDeleteTabs
                NOP
            ELSE
                result:Append(cChar)
            ENDIF
        CASE c'\r'   // CR
            IF lDeleteCR
                NOP
            ELSE
                result:Append(cChar)
            ENDIF
            lStartOfLine := TRUE
        CASE c'\n'   // LF
            IF lDeleteLF
                NOP
            ELSE
                result:Append(cChar)
                result:Append(cPreText)
            ENDIF
            lStartOfLine := TRUE
        OTHERWISE
            result:Append(cChar)
            lStartOfLine := FALSE
        END SWITCH
    NEXT
    cText := Result:ToString()
    IF ! lNoShow
        Qout(cText)
    ENDIF
    IF _AND(nFlags ,1) != 1
        // Output to _TEXT
        if _TEXT != -1
            local hFile as IntPtr
            hFile := IntPtr{ _TEXT}
            IF lNoShow .and. _AND(nFlags,2) == 2
                FWrite(hFile, System.Environment.NewLine)
            ENDIF
            FWrite(hFile, cText)
        ENDIF
    ENDIF
    RETURN cText
