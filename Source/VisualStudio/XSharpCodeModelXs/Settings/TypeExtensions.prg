//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Linq
USING System

BEGIN NAMESPACE XSharp.Settings

    STATIC CLASS TypeExtensions
        // Fields
        STATIC INTERNAL SystemToXSharp AS IDictionary<STRING, STRING>
        STATIC INTERNAL XSharpToSystem AS IDictionary<STRING, STRING>
        STATIC INTERNAL VulcanToSystem AS IDictionary<STRING, STRING>

        // Methods
        STATIC CONSTRUCTOR()
            SystemToXSharp  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            SystemToXSharp:Add(KnownTypes.SystemBoolean, "LOGIC")
            SystemToXSharp:Add(KnownTypes.SystemByte, "BYTE")
            SystemToXSharp:Add(KnownTypes.SystemChar, "CHAR")
            SystemToXSharp:Add(KnownTypes.SystemDouble, "REAL8")
            SystemToXSharp:Add(KnownTypes.SystemInt16, "SHORT")
            SystemToXSharp:Add(KnownTypes.SystemInt32, "INT")
            SystemToXSharp:Add(KnownTypes.SystemInt64, "INT64")
            SystemToXSharp:Add(KnownTypes.SystemObject, "OBJECT")
            SystemToXSharp:Add(KnownTypes.SystemSingle, "REAL4")
            SystemToXSharp:Add(KnownTypes.SystemString, "STRING")
            SystemToXSharp:Add(KnownTypes.SystemUInt16, "WORD")
            SystemToXSharp:Add(KnownTypes.SystemUInt32, "DWORD")
            SystemToXSharp:Add(KnownTypes.SystemUInt64, "UINT64")
            SystemToXSharp:Add(KnownTypes.SystemVoid, "VOID")
            SystemToXSharp:Add(KnownTypes.SystemIntPtr, "PTR")


            SystemToXSharp:Add(KnownTypes.VulcanCodeblock, "CODEBLOCK")
            SystemToXSharp:Add(KnownTypes.VulcanArray, "ARRAY")
            SystemToXSharp:Add(KnownTypes.VulcanDate, "DATE")
            SystemToXSharp:Add(KnownTypes.VulcanFloat, "FLOAT")
            SystemToXSharp:Add(KnownTypes.VulcanPSZ, "PSZ")
            SystemToXSharp:Add(KnownTypes.VulcanSymbol, "SYMBOL")
            SystemToXSharp:Add(KnownTypes.VulcanUsual, "USUAL")
            SystemToXSharp:Add(KnownTypes.VulcanWinBool, "LOGIC")

            SystemToXSharp:Add(KnownTypes.XSharpCodeblock, "CODEBLOCK")
            SystemToXSharp:Add(KnownTypes.XSharpArray, "ARRAY")
            SystemToXSharp:Add(KnownTypes.XSharpBinary, "BINARY")
            SystemToXSharp:Add(KnownTypes.XSharpCurrency, "CURRENCY")
            SystemToXSharp:Add(KnownTypes.XSharpDate, "DATE")
            SystemToXSharp:Add(KnownTypes.XSharpFloat, "FLOAT")
            SystemToXSharp:Add(KnownTypes.XSharpPSZ, "PSZ")
            SystemToXSharp:Add(KnownTypes.XSharpSymbol, "SYMBOL")
            SystemToXSharp:Add(KnownTypes.XSharpUsual, "USUAL")
            SystemToXSharp:Add(KnownTypes.XSharpWinBool, "LOGIC")
            SystemToXSharp:Add(KnownTypes.XSharpWinDate, "DATE")
            XSharpToSystem  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            VulcanToSystem  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            FOREACH VAR pair IN SystemToXSharp
                if !pair:Key:Contains("__WinBool")
                    if pair:Key:StartsWith("Vulcan")
                        IF ! VulcanToSystem:ContainsKey(pair:Value)
                            VulcanToSystem:Add(pair:Value, pair:Key)
                        ENDIF
                    else
                        IF ! XSharpToSystem:ContainsKey(pair:Value)
                            XSharpToSystem:Add(pair:Value, pair:Key)
                        ENDIF
                    endif
                endif
            NEXT
            XSharpToSystem:Add("LONG", KnownTypes.SystemInt32)
            XSharpToSystem:Add("LONGINT", KnownTypes.SystemInt32)
            XSharpToSystem:Add("SHORTINT", KnownTypes.SystemInt16)

        STATIC METHOD IsXSharpTypeName( SELF typeName as STRING) AS LOGIC
            RETURN XSharpToSystem:ContainsKey(typeName) .or. VulcanToSystem:ContainsKey(typeName)

        STATIC METHOD GetSystemTypeName( SELF typename AS STRING, lXSharpNames AS LOGIC) AS STRING
            LOCAL lHandled := TRUE AS LOGIC
            IF typename == null
                return typename
            ENDIF
            IF lXSharpNames
                if XSharpToSystem:ContainsKey(typename)
                    return XSharpToSystem[typename]
                endif
                IF typename:ToLower():StartsWith("array<")
                    typename := KnownTypes.XSharpArrayBase+"<"+typename:Substring(6)
                ENDIF

            ELSE
                if VulcanToSystem:ContainsKey(typename)
                    return VulcanToSystem[typename]
                endif
                if XSharpToSystem:ContainsKey(typename)
                    return XSharpToSystem[typename]
                endif
            ENDIF
            if typename:EndsWith("]") .and. typename:Contains("[")
                return KnownTypes.SystemArray
            endif
            var index := typename:IndexOf("<")
            if index > 0 && index < typename:Length
                var left     := typename:Substring(0, index)
                var right    := typename:Substring(index+1)
                if (right.Length > 0)
                    right        := right:Substring(0, right:Length-1)
                    left += "`"
                    if right.Contains(",")
                        var elements := right:Split( <char>{','}, StringSplitOptions.RemoveEmptyEntries)
                        typename     := left+elements:Length:ToString()
                    else
                        typename     := left+"1"
                    endif
                 endif
            endif
            // In case of a nested type, replace the forward slash with a dot
            if typename:Contains("/")
                typename := typename.Replace("/",".")
            endif
            RETURN typename


    STATIC METHOD GetTypeSuffix(typeName REF STRING) AS STRING
        LOCAL suffix AS STRING
        suffix := ""
        if typeName:EndsWith("]")
            var pos := typeName:IndexOf("[")
            suffix  := typeName:Substring(pos)
            typeName := typeName:Substring(0, pos)
        endif
        if typeName:EndsWith("&")
            typeName := typeName:Substring(0, typeName:Length -1)
            suffix := "&"
        endif
        if typeName:EndsWith("*")
            typeName := typeName:Substring(0, typeName:Length -1)
            suffix := "*"
        endif
        return suffix

    STATIC METHOD GetXSharpTypeName( SELF typeName AS STRING) AS STRING
        var suffix := GetTypeSuffix(REF typeName)
        if typeName:IndexOf("/") >= 0
            typeName := typeName:Replace('/','.')
        endif
        IF (SystemToXSharp:ContainsKey(typeName))
            //
            typeName := SystemToXSharp[typeName]
        ENDIF
        VAR pos := typeName:IndexOf("<")
        IF pos > 0
            VAR lhs := typeName:Substring(0, pos)
            VAR rhs := typeName:Substring(pos+1)
            pos := lhs:IndexOf("`")
            IF pos > 0
                lhs := lhs:Substring(0,pos)
            ENDIF
            VAR parts := rhs:Split(<Char>{',','>'},StringSplitOptions.RemoveEmptyEntries)
            VAR delim := "<"
            FOREACH VAR part IN parts
                lhs += delim
                delim := ","
                lhs += part:GetXSharpTypeName()
            NEXT
            lhs += ">"
            typeName := lhs
        ENDIF
        IF suffix == "*"
            if typeName == "VOID"
                typeName := "PTR"
                suffix := ""
            else
                suffix := " PTR"
            endif
        ENDIF
        if suffix:Length > 0
             typeName += suffix
        endif
        RETURN typeName


    END CLASS

END NAMESPACE


BEGIN NAMESPACE System
CLASS StringExtensions
    STATIC METHOD ReplaceEx(SELF input AS STRING, search AS STRING, replace AS STRING, comparison AS System.StringComparison) AS STRING
        VAR stringBuilder := System.Text.StringBuilder{}
        VAR lastIndex := 0
        VAR currIndex := input:IndexOf(search, comparison)
        DO WHILE currIndex != -1

            stringBuilder:Append(input:Substring(lastIndex, currIndex - lastIndex))
            stringBuilder:Append(replace)
            currIndex += search.Length
            lastIndex := currIndex
            currIndex := input:IndexOf(search, currIndex, comparison)
        ENDDO
        stringBuilder:Append(input.Substring(lastIndex))
        RETURN stringBuilder.ToString()
END CLASS
END NAMESPACE

