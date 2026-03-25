//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Reflection

/// <include file="XSharp.XPP.Docs.xml" path="doc/SetCollation/*" />
FUNCTION SetCollation(nCollation) AS LONG CLIPPER
    LOCAL nOld := @@Set(Set.Collation) AS LONG
    IF PCount() > 0 
        IF IsNumeric(nCollation)
            SetCollationTable(nCollation)
        ELSEIF IsString(nCollation)
            XSharp.Core.Functions.SetCollation((STRING) nCollation)
        ELSEIF IsSymbol(nCollation)
            XSharp.Core.Functions.SetCollation((SYMBOL) nCollation)
        endif
    ENDIF
    RETURN nOld


/// <include file="XSharp.XPP.Docs.xml" path="doc/SetCollationTable/*" />
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



