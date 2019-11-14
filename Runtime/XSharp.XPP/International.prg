// Aliases to functions in XSharp.RT

USING System.Reflection

PROCEDURE InitCollation INIT3
    SetCollation(XppCollations.System)
    SET(Set.CharSet, CHARSET_ANSI)
    RETURN

FUNCTION SetCollation(nCollation) AS LONG
    LOCAL nOld := Set(Set.Collation) AS LONG
    IF PCount() > 0 .AND. IsNumeric(nCollation)
        SetCollationTable(nCollation)
    ENDIF
    RETURN nOld

FUNCTION SetCollationTable(nCollation, aTable) AS ARRAY
    LOCAL liEnum AS LONG
    LOCAL aCollation AS ARRAY
    IF PCount() >= 1
        liEnum := (LONG) nCollation
        IF IsArray(aTable)
            aCollation := aTable
        ENDIF
    ELSE
        liEnum := SET(Set.Collation) 
    ENDIF
    IF liEnum < XppCollations.Ascii .OR. liEnum > XppCollations.User
        liEnum := XppCollations.Ascii
    ENDIF
    VAR nEnum := (XppCollations) liEnum
    LOCAL aBytes := XSharp.RuntimeState.CollationTable AS BYTE[]
    IF liEnum != SET(Set.Collation) 
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
                    tableName := "Ansi"+tablename
                ELSE
                    tableName := "Oem"+tablename
                ENDIF
            ENDIF
            LOCAL oType := typeof(XSharp.XPP.Collations) AS System.Type
            LOCAL oProp := oType:GetProperty(tableName, BindingFlags.Static| BindingFlags.Public | BindingFlags.NonPublic| BindingFlags.IgnoreCase) AS PropertyInfo
            IF oProp != NULL
                aBytes := (BYTE[]) oProp:GetValue(NULL)
                XSharp.RuntimeState.CollationTable := aBytes
                XSharp.RuntimeState.CollationMode  := CollationMode.XPP
                SET(Set.Collation, liEnum)
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
        FOR VAR nI := 1 TO alen(aCollation)
            aCollation[nI] := chr((DWORD) nI-1)
        NEXT
        ASort(aCollation)
        FOR VAR nI := 1 TO alen(aCollation)
            aCollation[nI] := asc(aCollation[nI])
        NEXT
        
    ENDIF
    RETURN aCollation

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

           

