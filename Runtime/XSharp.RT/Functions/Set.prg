//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <summary>
/// Changes and/or reads a system setting. 
/// </summary>
/// <param name="nDefine">Is a positive integer identifying a system setting or system variable.
/// This should match the values from the Set enumerated type.</param>
/// <param name="newValue">The optional expression can specify a new value for a system setting.
/// The data type is dependent on the system setting designated by <paramref name="nDefine" />.</param>
/// <returns>When Set() is called without the argument <paramref name="newValue" /> ,
/// the function returns the current system setting designated by <paramref name="nDefine" /> .
/// If <paramref name="newValue" /> is specified, the corresponding system setting is set to <paramref name="newValue" />
/// and the value of the old setting is returned. 
/// </returns>
/// <seealso cref="T:XSharp.Set" />
/// <remarks>If you are coming from XHarbour or Xbase++ please don't use set.ch for the value of <paramref name="nDefine" />
/// because there are some differences between the values in this header file and the values used inside X#. </remarks>
FUNCTION Set(nDefine, newValue) AS USUAL CLIPPER
    LOCAL state AS XSharp.RuntimeState
    LOCAL oOld  := NULL AS OBJECT
    LOCAL nSetting AS XSharp.Set
    IF ! IsNumeric(nDefine)
        RETURN NIL
    ENDIF
    nSetting := nDefine
    state := XSharp.RuntimeState.GetInstance()
    IF state:Settings:ContainsKey(nSetting)
        oOld := state:Settings[nSetting]
    ENDIF
    IF PCount() > 1
        LOCAL oNew := newValue AS OBJECT
        IF oOld != NULL_OBJECT
            IF oOld IS LOGIC .AND. oNew IS STRING VAR cNew
                // XBase++ uses ON / OFF strings in stead of TRUE and FALSE
                state:Settings[nSetting] := cNew:ToUpper() == "ON"
            ELSE
               TRY
                    oNew := System.Convert.ChangeType( oNew, oOld:GetType())
                    state:Settings[nSetting] := oNew
                CATCH
                    NOP // can't convert, so ignore assignment
                END TRY
            ENDIF
        ENDIF
        // Map SET_CHARSET to SetAnsi
        IF nSetting == _SET_CHARSET .AND. IsNumeric(newValue)
            state:Settings[Set.Ansi] := newValue == 0  // Ansi = 0, Oem == 1
        ENDIF
    ENDIF
    RETURN oOld
            
    


