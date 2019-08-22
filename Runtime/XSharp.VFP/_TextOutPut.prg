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
