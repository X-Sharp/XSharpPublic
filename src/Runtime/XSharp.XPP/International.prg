//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Reflection

/// <summary>Query current and/or set new active collation.</summary>
/// <param name="nCollation">the numeric identifier for the collation table to be selected. Must be one of the values from the XppCollations enum of a simple number</param>
/// <returns>The numeric identifier of the active collation table </returns>
/// <seealso cref='XppCollations' />
/// <seealso cref='SetCollationTable' />
FUNCTION SetCollation(nCollation) AS LONG CLIPPER
    LOCAL nOld := @@Set(Set.Collation) AS LONG
    IF PCount() > 0 .AND. IsNumeric(nCollation)
        SetCollationTable(nCollation)
    ENDIF
    RETURN nOld


/// <summary>Query current and/or set user defined collation table .</summary>
/// <param name="nCollation">the numeric identifier for the collation table to be selected. Must be one of the values from the XppCollations enum of a simple number</param>
/// <param name="aTable">a one dimensional array with 256 elements. It must contain numeric values representing the weighing factors for single characters.
/// The ASCII value of a character plus 1 defines the array element that contains the weighing factor for that character. </param>
/// <returns>a one dimensional array holding the weighing factors of characters for the active collation. </returns>
/// <seealso cref='XppCollations' />
/// <seealso cref='SetCollation' />
FUNCTION SetCollationTable(nCollation, aTable) AS ARRAY CLIPPER
    LOCAL liEnum AS LONG
    LOCAL aCollation AS ARRAY
    IF PCount() >= 1
        liEnum := (LONG) nCollation
        IF IsArray(aTable)
            aCollation := aTable
        ENDIF
    ELSE
        liEnum := @@Set(Set.Collation)
    ENDIF
    IF liEnum < XppCollations.Ascii .OR. liEnum > XppCollations.User
        liEnum := XppCollations.Ascii
    ENDIF
    VAR nEnum := (XppCollations) liEnum
    LOCAL aBytes := XSharp.RuntimeState.CollationTable AS BYTE[]
    IF liEnum != XSharp.RuntimeState.GetValue<LONG>(Set.Collation)
        XSharp.RuntimeState.CollationTable := NULL
        XSharp.RuntimeState.CollationMode  := CollationMode.Windows
//        IF nEnum == XppCollations.System
//            aBytes := NULL
//        ELSE
            XSharp.RuntimeState.CollationTable := NULL
            XSharp.RuntimeState.CollationMode  := CollationMode.Windows
            LOCAL tableName := nEnum:ToString() AS STRING
            IF nEnum == XppCollations.System
                IF SetAnsi()
                    tableName := "Ansi"+tableName
                ELSE
                    tableName := "Oem"+tableName
                ENDIF
            ENDIF
            LOCAL oType := typeof(XSharp.XPP.XPPCollations) AS System.Type
            LOCAL oProp := oType:GetProperty(tableName, BindingFlags.Static| BindingFlags.Public | BindingFlags.NonPublic| BindingFlags.IgnoreCase) AS PropertyInfo
            IF oProp != NULL
                aBytes := (BYTE[]) oProp:GetValue(NULL)
                XSharp.RuntimeState.CollationTable := aBytes
                XSharp.RuntimeState.CollationMode  := CollationMode.Xpp
                XSharp.RuntimeState.SetValue(Set.Collation, liEnum)
            ENDIF
//         ENDIF
    ENDIF
    IF aBytes != NULL
        aCollation := ArrayNew((DWORD) aBytes:Length)
        FOR VAR nI := 1 TO aBytes:Length
            aCollation[nI] := aBytes[nI]
        NEXT
    ELSE
        // Create aCollation from Windows Sort Routine
        aCollation := ArrayNew(256)
        FOR VAR nI := 1 TO ALen(aCollation)
            aCollation[nI] := Chr((DWORD) nI-1)
        NEXT
        ASort(aCollation)
        FOR VAR nI := 1 TO ALen(aCollation)
            aCollation[nI] := Asc(aCollation[nI])
        NEXT

    ENDIF
    RETURN aCollation



