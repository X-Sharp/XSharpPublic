//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Reflection

/// <summary>Query current and/or set new active collation.</summary>
/// <param name="nCollation">the numeric identifier for the collation table to be selected. Must be one of the values from the XppCollations enum of a simple number</param>
/// <returns>The numeric identifier of the active collation table </returns>
/// <seealso cref='T:XSharp.XPP.XppCollations' />
/// <seealso cref='M:XSharp.XPP.Functions.SetCollationTable(XSharp.__Usual,XSharp.__Usual)' />
FUNCTION SetCollation(nCollation) AS LONG
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
/// <seealso cref='T:XSharp.XPP.XppCollations' />
/// <seealso cref='M:XSharp.XPP.Functions.SetCollation(XSharp.__Usual)' />
FUNCTION SetCollationTable(nCollation, aTable) AS ARRAY
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
    IF liEnum != @@Set(Set.Collation) 
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
            LOCAL oType := typeof(XSharp.XPP.Collations) AS System.Type
            LOCAL oProp := oType:GetProperty(tableName, BindingFlags.Static| BindingFlags.Public | BindingFlags.NonPublic| BindingFlags.IgnoreCase) AS PropertyInfo
            IF oProp != NULL
                aBytes := (BYTE[]) oProp:GetValue(NULL)
                XSharp.RuntimeState.CollationTable := aBytes
                XSharp.RuntimeState.CollationMode  := CollationMode.Xpp
                @@Set(Set.Collation, liEnum)
            ENDIF
//         ENDIF
    ENDIF
    IF aBytes != NULL
        aCollation := {}
        FOR VAR nI := 1 TO aBytes:Length
            AAdd(aCollation, aBytes[nI])
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

/// <summary>List of possible values for XBase++ collations</summary>
/// <remarks>The System enumeration value is represented by 2 different sort orders, depending on the current Ansi or Charmode setting.</remarks>
ENUM XppCollations
    MEMBER Ascii          :=   -1
    MEMBER System         :=   0    
    MEMBER German         :=   1
    MEMBER British        :=   2
    MEMBER American       :=   2
    MEMBER Finnish        :=   3
    MEMBER French         :=   4
    MEMBER Danish         :=   5
    MEMBER Greek437       :=   6
    MEMBER Greek851       :=   7
    MEMBER Icelandic850   :=   8
    MEMBER Icelandic861   :=   9
    MEMBER Italian        :=  10
    MEMBER Norwegian      :=  11
    MEMBER Portuguese     :=  12
    MEMBER Spanish        :=  13
    MEMBER Swedish        :=  14
    MEMBER Dutch          :=  15
    MEMBER User           :=  16
END ENUM    

           

