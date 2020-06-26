//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text


/// <summary>Retrieves the 1 based position of the first upper case letter in a string. </summary>
/// <returns>Returns the position of the first upper case letter in a string as a numeric value, or ZERO when there is no uppercase letter in the string. </returns>
/// <param name="cString">String to check</param>
/// <param name="lNoLetter">When TRUE is passed then the first character is returned which is not changed by function Lower(). Defaults to FALSE.</param>
/// <param name="nIgnoreCharsFromLeft">Number of characters that must be ignored. Defaults to 0.</param>
/// <remarks>In X# A letter is defined as a character for which the Char.IsLetter() method returns TRUE. 
/// If <paramref name="lNoLetter"/> is set to TRUE  the function returns the position of the first character which is not a letter. </remarks>
/// <seealso cref="M:XSharp.XPP.Functions.PosLower(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosAlpha(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>

FUNCTION PosUpper(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG
    EnforceType(cString, STRING)  
    DEFAULT( REF lNoLetter, FALSE)
    DEFAULT( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
            IF Char.IsLetter(c)
                IF Char.IsUpper(c)
                    RETURN TRUE
                ENDIF
            ELSEIF noLetter
                RETURN TRUE
            ENDIF
            RETURN FALSE
            } )
    
    
/// <summary>Retrieves the 1 based position of the first lower case letter in a string. </summary>
/// <returns>Returns the position of the first lower case letter in a string as a numeric value, or ZERO when there is no uppercase letter in the string. </returns>
/// <param name="cString">String to check</param>
/// <param name="lNoLetter">When TRUE is passed then the first character is returned which is not changed by function Upper(). Defaults to FALSE.</param>
/// <param name="nIgnoreCharsFromLeft">Number of characters that must be ignored. Defaults to 0.</param>
/// <remarks>In X# A letter is defined as a character for which the Char.IsLetter() method returns TRUE. 
/// If <paramref name="lNoLetter"/> is set to TRUE  the function returns the position of the first character which is not a letter. </remarks>
/// <seealso cref="M:XSharp.XPP.Functions.PosUpper(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosAlpha(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>


FUNCTION PosLower(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG
    EnforceType(cString, STRING)  
    DEFAULT( REF lNoLetter, FALSE)
    DEFAULT( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
        IF Char.IsLetter(c)
            IF Char.IsLower(c)
                RETURN TRUE
            ENDIF
        ELSEIF noLetter
            RETURN TRUE
        ENDIF
        RETURN FALSE
        } )
    
    
    
    

/// <summary>Retrieves the 1 based position of the first letter in a string. </summary>
/// <returns>Returns the position of the first letter in a string as a numeric value, or ZERO when there is no uppercase letter in the string. </returns>
/// <param name="cString">String to check</param>
/// <param name="lNoLetter">When TRUE is passed then the first character is returned which is not a not a letter. Defaults to FALSE.</param>
/// <param name="nIgnoreCharsFromLeft">Number of characters that must be ignored. Defaults to 0.</param>
/// <remarks>In X# A letter is defined as a character for which the Char.IsLetter() method returns TRUE. 
/// If <paramref name="lNoLetter"/> is set to TRUE the function returns the position of the first character which is not a letter. </remarks>
/// <seealso cref="M:XSharp.XPP.Functions.PosUpper(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosLower(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>


FUNCTION PosAlpha(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG
    EnforceType(cString, STRING)  
    DEFAULT( REF lNoLetter, FALSE)
    DEFAULT( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
                                IF Char.IsLetter(c)
                                    RETURN TRUE
                                ELSEIF noLetter
                                    RETURN TRUE
                                ENDIF
                                RETURN FALSE
                                } )
    
INTERNAL DELEGATE PosDelegate( cChar AS CHAR) AS LOGIC

    INTERNAL FUNCTION PosWorker(sString AS STRING, ignoreChars AS LONG, delCheck AS PosDelegate) AS LONG
    FOR VAR nI := ignoreChars TO sString:Length
    VAR cChar := sString[nI]
        IF delCheck(cChar)
            RETURN nI+1
        ENDIF
        NEXT
    RETURN 0
    

/// <summary>Replaces a single character at a specified position in a string. </summary>
/// <returns>Returns the modified string. </returns>
/// <param name="cString">String to check</param>
/// <param name="uChar">The new character to insert into the string</param>
/// <param name="nPosition">1 based position to change. Defaults to Len(<paramref name="cString"/>).</param>
/// <seealso cref="M:XSharp.XPP.Functions.PosDel(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosIns(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosRepl(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
    
FUNCTION PosChar(cString, uChar, nPosition ) AS STRING
    EnforceType(cString, STRING)
    LOCAL sReplace AS STRING
    LOCAL sSource  AS STRING
        
    IF IsString(uChar)
        sReplace := uChar
    ELSEIF IsNumeric(uChar)
        sReplace := Chr(uChar)
    ELSE
        VAR cMessage := "Expected type: Numeric or String actual type "+ ((__UsualType) UsualType(uChar)):ToString()
        THROW Error.DataTypeError(ProcName(), nameof(uChar), 2, uChar, cMessage)
    ENDIF
    sSource := cString
    DEFAULT(nPosition, SLen(sSource))
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    IF nPosition <= sb:Length
        sb[nPosition-1] := sReplace[0]
    ENDIF
    RETURN sb:ToString()
    
   

/// <summary>Removes characters at a specified position in a string. </summary>
/// <returns>Returns the modified string. </returns>
/// <param name="cString">String to check</param>
/// <param name="nStartPos">1 based position to start deleting from. Defaults to Len(<paramref name="cString"/>).</param>
/// <param name="nDeleteLen">The number of characters to delete.</param>
/// <seealso cref="M:XSharp.XPP.Functions.PosChar(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosIns(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosRepl(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>


FUNCTION PosDel(cString, nStartPos, nDeleteLen ) AS STRING
    EnforceType(cString, STRING)
    EnforceType(nDeleteLen,LONG)
    LOCAL sSource  AS STRING
    sSource := cString
    DEFAULT(nStartPos, SLen(sSource))
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    nDeleteLen := Math.Min(sSource:Length - nStartPos+1, nDeleteLen)
    sb:Remove(nStartPos-1,nDeleteLen)
    RETURN sb:ToString()
    
    
    

/// <summary>Inserts a character string into a string at a specified position.  </summary>
/// <returns>Returns the modified string. </returns>
/// <param name="cString">String to update</param>
/// <param name="cInsertString">The string to insert</param>
/// <param name="nPosition">1 based position where the string must be inserted. Defaults to Len(<paramref name="cString"/>)+1.</param>
/// <seealso cref="M:XSharp.XPP.Functions.PosChar(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosDel(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosRepl(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>

FUNCTION PosIns(cString, cInsertString, nPosition ) AS STRING
    EnforceType(cString, STRING)
    EnforceType(cInsertString,STRING)
    LOCAL sSource  AS STRING
    sSource := cString
    DEFAULT(nPosition, SLen(sSource))   // not + 1, StringBuilder is zero based
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    sb:Insert(nPosition, cInsertString, 1)
    RETURN sb:ToString()
    
    
    

/// <summary>Inserts a character string into a string at a specified position.</summary>
/// <returns>Returns the modified string.</returns>
/// <param name="cString">String to update</param>
/// <param name="cReplace">The string to insert.</param>
/// <param name="nStartPos">1 based position where the string must be inserted. </param>
/// <remarks>If the parameter <paramref name="nStartPos"/> is not specified the function
/// replaces Len( <paramref name="cReplace"/> ) characters at the end of <paramref name="cString"/>.</remarks>
/// <seealso cref="M:XSharp.XPP.Functions.PosChar(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosDel(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
/// <seealso cref="M:XSharp.XPP.Functions.PosIns(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)"/>
FUNCTION PosRepl(cString, cReplace, nStartPos ) AS STRING
    EnforceType(cString, STRING)
    EnforceType(cReplace,STRING)
    LOCAL sSource  AS STRING
    LOCAL sReplace AS STRING
    sSource     := cString
    sReplace    := cReplace
    DEFAULT(nStartPos, sSource:Length - sReplace:Length+1)
    nStartPos := Math.Max(nStartPos, 1L)
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    VAR nToDel := sReplace:Length  
    nToDel := Math.Min(sSource:Length-nStartPos+1, nToDel)
    sb:Remove(nStartPos-1, nToDel)
    sb:Insert(nStartPos-1, sReplace, 1)
    RETURN sb:ToString()
