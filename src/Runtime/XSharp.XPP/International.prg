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


FUNCTION SetLocale(nDefine as LONG) AS STRING
    SWITCH nDefine
    CASE NLS_SDECIMAL
        RETURN Chr(SetDecimalSep())
    CASE NLS_STHOUSAND
        RETURN Chr(SetThousandSep())
    CASE NLS_SLIST
        return System.Globalization.CultureInfo.CurrentCulture:TextInfo:ListSeparator
    CASE NLS_SYES
        return __CavoStr(VOErrors.RT_MSG_SHORT_YES)
    CASE NLS_SNO
        return __CavoStr(VOErrors.RT_MSG_SHORT_NO)
    CASE NLS_STIME
        RETURN Chr(SetTimeSep())
    CASE NLS_ITIME
        RETURN IIF(SetAmPm(), "0", "1")
    CASE NLS_SDATE
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:DateSeparator
    CASE NLS_IDATE
    CASE NLS_S1159
        RETURN SetAMExt()
    CASE NLS_S2359
        RETURN SetPMExt()
    CASE NLS_SCURRENCY
    CASE NLS_ICOUNTRY
        RETURN ""
    CASE NLS_IANSICP
        return NTrim(XSharp.RuntimeState.WinCodePage)
    CASE NLS_IOEMCP
        return NTrim(XSharp.RuntimeState.DosCodePage)
    CASE NLS_SDAYNAME1
    CASE NLS_SDAYNAME2
    CASE NLS_SDAYNAME3
    CASE NLS_SDAYNAME4
    CASE NLS_SDAYNAME5
    CASE NLS_SDAYNAME6
    CASE NLS_SDAYNAME7
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:DayNames[nDefine - NLS_SDAYNAME1+1]
    CASE NLS_SFDAYNAME1
    CASE NLS_SFDAYNAME2
    CASE NLS_SFDAYNAME3
    CASE NLS_SFDAYNAME4
    CASE NLS_SFDAYNAME5
    CASE NLS_SFDAYNAME6
    CASE NLS_SFDAYNAME7
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:DayNames[nDefine - NLS_SFDAYNAME1+1]
    CASE NLS_SMONTHNAME1
    CASE NLS_SMONTHNAME2
    CASE NLS_SMONTHNAME3
    CASE NLS_SMONTHNAME4
    CASE NLS_SMONTHNAME5
    CASE NLS_SMONTHNAME6
    CASE NLS_SMONTHNAME7
    CASE NLS_SMONTHNAME8
    CASE NLS_SMONTHNAME9
    CASE NLS_SMONTHNAME10
    CASE NLS_SMONTHNAME11
    CASE NLS_SMONTHNAME12
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:MonthNames[nDefine - NLS_SMONTHNAME1+1]
    CASE NLS_SFMONTHNAME1
    CASE NLS_SFMONTHNAME2
    CASE NLS_SFMONTHNAME3
    CASE NLS_SFMONTHNAME4
    CASE NLS_SFMONTHNAME5
    CASE NLS_SFMONTHNAME6
    CASE NLS_SFMONTHNAME7
    CASE NLS_SFMONTHNAME8
    CASE NLS_SFMONTHNAME9
    CASE NLS_SFMONTHNAME10
    CASE NLS_SFMONTHNAME11
    CASE NLS_SFMONTHNAME12
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:MonthGenitiveNames[nDefine - NLS_SFMONTHNAME1+1]
    CASE NLS_SABBREVDAYNAME1
    CASE NLS_SABBREVDAYNAME2
    CASE NLS_SABBREVDAYNAME3
    CASE NLS_SABBREVDAYNAME4
    CASE NLS_SABBREVDAYNAME5
    CASE NLS_SABBREVDAYNAME6
    CASE NLS_SABBREVDAYNAME7
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:AbbreviatedDayNames[nDefine - NLS_SABBREVDAYNAME1+1]
    CASE NLS_SABBREVMONTHNAME1
    CASE NLS_SABBREVMONTHNAME2
    CASE NLS_SABBREVMONTHNAME3
    CASE NLS_SABBREVMONTHNAME4
    CASE NLS_SABBREVMONTHNAME5
    CASE NLS_SABBREVMONTHNAME6
    CASE NLS_SABBREVMONTHNAME7
    CASE NLS_SABBREVMONTHNAME8
    CASE NLS_SABBREVMONTHNAME9
    CASE NLS_SABBREVMONTHNAME10
    CASE NLS_SABBREVMONTHNAME11
    CASE NLS_SABBREVMONTHNAME12
        return System.Globalization.DateTimeFormatInfo.CurrentInfo:AbbreviatedMonthNames[nDefine - NLS_SABBREVMONTHNAME1+1]
    END SWITCH
    RETURN ""

FUNCTION SetLocale(nDefine as LONG, cNewSetting as STRING) AS STRING
    var oldValue := SetLocale(nDefine)
    SWITCH nDefine
    CASE NLS_SDECIMAL
        SetDecimalSep(Asc(cNewSetting))
    CASE NLS_STHOUSAND
        SetThousandSep(Asc(cNewSetting))
    CASE NLS_STIME
        SetTimeSep(Asc(cNewSetting))
    CASE NLS_ITIME
        SetAmPm(Left(cNewSetting, 1) == "0")
    CASE NLS_SLIST
        System.Globalization.CultureInfo.CurrentCulture:TextInfo:ListSeparator := cNewSetting
    CASE NLS_S1159
        SetAMExt(cNewSetting)
    CASE NLS_S2359
        SetPMExt(cNewSetting)
    CASE NLS_SDATE
        System.Globalization.DateTimeFormatInfo.CurrentInfo:DateSeparator := cNewSetting
    END SWITCH
    RETURN oldValue

