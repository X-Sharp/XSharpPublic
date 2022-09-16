//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Linq
USING System
BEGIN NAMESPACE XSharpModel

    STATIC CLASS TypeExtensions
        // Fields
        STATIC INTERNAL SystemToXSharp AS IDictionary<STRING, STRING>
        STATIC INTERNAL XSharpToSystem AS IDictionary<STRING, STRING>
        STATIC INTERNAL VulcanToSystem AS IDictionary<STRING, STRING>

        // Methods
        STATIC CONSTRUCTOR()
            SystemToXSharp  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            SystemToXSharp:Add(XSharpTypeNames.SystemBoolean, "LOGIC")
            SystemToXSharp:Add(XSharpTypeNames.SystemByte, "BYTE")
            SystemToXSharp:Add(XSharpTypeNames.SystemChar, "CHAR")
            SystemToXSharp:Add(XSharpTypeNames.SystemDouble, "REAL8")
            SystemToXSharp:Add(XSharpTypeNames.SystemInt16, "SHORT")
            SystemToXSharp:Add(XSharpTypeNames.SystemInt32, "INT")
            SystemToXSharp:Add(XSharpTypeNames.SystemInt64, "INT64")
            SystemToXSharp:Add(XSharpTypeNames.SystemObject, "OBJECT")
            SystemToXSharp:Add(XSharpTypeNames.SystemSingle, "REAL4")
            SystemToXSharp:Add(XSharpTypeNames.SystemString, "STRING")
            SystemToXSharp:Add(XSharpTypeNames.SystemUInt16, "WORD")
            SystemToXSharp:Add(XSharpTypeNames.SystemUInt32, "DWORD")
            SystemToXSharp:Add(XSharpTypeNames.SystemUInt64, "UINT64")
            SystemToXSharp:Add(XSharpTypeNames.SystemVoid, "VOID")
            SystemToXSharp:Add(XSharpTypeNames.SystemIntPtr, "PTR")


            SystemToXSharp:Add(XSharpTypeNames.VulcanCodeblock, "CODEBLOCK")
            SystemToXSharp:Add(XSharpTypeNames.VulcanArray, "ARRAY")
            SystemToXSharp:Add(XSharpTypeNames.VulcanDate, "DATE")
            SystemToXSharp:Add(XSharpTypeNames.VulcanFloat, "FLOAT")
            SystemToXSharp:Add(XSharpTypeNames.VulcanPSZ, "PSZ")
            SystemToXSharp:Add(XSharpTypeNames.VulcanSymbol, "SYMBOL")
            SystemToXSharp:Add(XSharpTypeNames.VulcanUsual, "USUAL")
            SystemToXSharp:Add(XSharpTypeNames.VulcanWinBool, "LOGIC")

            SystemToXSharp:Add(XSharpTypeNames.XSharpCodeblock, "CODEBLOCK")
            SystemToXSharp:Add(XSharpTypeNames.XSharpArray, "ARRAY")
            SystemToXSharp:Add(XSharpTypeNames.XSharpBinary, "BINARY")
            SystemToXSharp:Add(XSharpTypeNames.XSharpCurr, "CURRENCY")
            SystemToXSharp:Add(XSharpTypeNames.XSharpDate, "DATE")
            SystemToXSharp:Add(XSharpTypeNames.XSharpFloat, "FLOAT")
            SystemToXSharp:Add(XSharpTypeNames.XSharpPSZ, "PSZ")
            SystemToXSharp:Add(XSharpTypeNames.XSharpSymbol, "SYMBOL")
            SystemToXSharp:Add(XSharpTypeNames.XSharpUsual, "USUAL")
            SystemToXSharp:Add(XSharpTypeNames.XSharpWinBool, "LOGIC")
            SystemToXSharp:Add(XSharpTypeNames.XSharpWinDate, "DATE")
            XSharpToSystem  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            VulcanToSystem  := XDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            FOREACH VAR pair IN SystemToXSharp
                if pair:Key:StartsWith("Vulcan")
                    IF ! VulcanToSystem:ContainsKey(pair:Value)
                        VulcanToSystem:Add(pair:Value, pair:Key)
                    ENDIF
                else
                    IF ! XSharpToSystem:ContainsKey(pair:Value)
                        XSharpToSystem:Add(pair:Value, pair:Key)
                    ENDIF
                endif
            NEXT
            XSharpToSystem:Add("LONG", "System.Int32")
            XSharpToSystem:Add("LONGINT", "System.Int32")
            XSharpToSystem:Add("SHORTINT", "System.Int16")

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
                    typename := "XSharp.__ArrayBase<"+typename:Substring(6)
                ENDIF

            ELSE
                if VulcanToSystem:ContainsKey(typename)
                    return VulcanToSystem[typename]
                endif
                if XSharpToSystem:ContainsKey(typename)
                    return XSharpToSystem[typename]
                endif
            ENDIF
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
        if typeName:EndsWith("[]")
            typeName := typeName:Substring(0, typeName:Length -2)
            suffix := "[]"
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
            VAR parts := rhs:Split(",>":ToCharArray(),StringSplitOptions.RemoveEmptyEntries)
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

    STATIC METHOD GetXSharpTypeName( SELF sysType AS Mono.Cecil.TypeReference) AS STRING
        var fullName := sysType:FullName
        IF (fullName == NULL)
            fullName := sysType:Name
        ENDIF
        var suffix := GetTypeSuffix(ref fullName)
        fullName := fullName:GetXSharpTypeName()
        // Maybe it's a Raw format ?
        LOCAL genMarker := fullName:IndexOf('`') AS INT
        IF (genMarker > -1)
            // First extract the type
            LOCAL genTypeName := fullName:Substring(0, genMarker) AS STRING
            VAR genericString := "<"
            VAR GenericParameters := sysType:GenericParameters
            LOCAL first := TRUE AS LOGIC
            FOREACH VAR genArg IN GenericParameters
                IF first
                    genericString += genArg:Name
                    first := FALSE
                ELSE
                    genericString += "," + genArg:Name
                ENDIF
            NEXT
            //
            genericString += ">"
            fullName := genTypeName + genericString
        ENDIF
        if suffix:Length > 0
            fullName += suffix
        endif
        RETURN fullName

    END CLASS

END NAMESPACE

