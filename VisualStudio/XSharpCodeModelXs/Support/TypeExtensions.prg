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

        // Methods
        STATIC CONSTRUCTOR()
            SystemToXSharp  := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            SystemToXSharp:Add("System.Boolean", "LOGIC")
            SystemToXSharp:Add("System.Byte", "BYTE")
            SystemToXSharp:Add("System.String", "STRING")
            SystemToXSharp:Add("System.Char", "CHAR")
            SystemToXSharp:Add("System.Double", "REAL8")
            SystemToXSharp:Add("System.Int16", "SHORT")
            SystemToXSharp:Add("System.Int32", "INT")

            SystemToXSharp:Add("System.Int64", "INT64")
            SystemToXSharp:Add("System.Object", "OBJECT")
            SystemToXSharp:Add("System.Single", "REAL4")
            SystemToXSharp:Add("System.UInt16", "WORD")
            SystemToXSharp:Add("System.UInt32", "DWORD")
            SystemToXSharp:Add("System.UInt64", "UINT64")
            SystemToXSharp:Add("System.Void", "VOID")
            SystemToXSharp:Add("XSharp._CodeBlock", "CODEBLOCK")
            SystemToXSharp:Add("XSharp.__Array", "ARRAY")
            SystemToXSharp:Add("XSharp.__Psz", "PSZ")
            SystemToXSharp:Add("XSharp.__Symbol", "SYMBOL")
            SystemToXSharp:Add("XSharp.__Usual", "USUAL")
            SystemToXSharp:Add("XSharp.__VODate", "DATE")
            SystemToXSharp:Add("XSharp.__VOFloat", "FLOAT")
            SystemToXSharp:Add("XSharp.__Date", "DATE")
            SystemToXSharp:Add("XSharp.__Float", "FLOAT")
            SystemToXSharp:Add("XSharp.__WinBool", "LOGIC")
            SystemToXSharp:Add("Vulcan._CodeBlock", "CODEBLOCK")
            SystemToXSharp:Add("Vulcan.__Array", "ARRAY")
            SystemToXSharp:Add("Vulcan.__Psz", "PSZ")
            SystemToXSharp:Add("Vulcan.__Symbol", "SYMBOL")
            SystemToXSharp:Add("Vulcan.__Usual", "USUAL")
            SystemToXSharp:Add("Vulcan.__VODate", "DATE")
            SystemToXSharp:Add("Vulcan.__VOFloat", "FLOAT")
            SystemToXSharp:Add("Vulcan.__WinBool", "LOGIC")
            XSharpToSystem  := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
            FOREACH VAR pair IN SystemToXSharp
               IF ! XSharpToSystem:ContainsKey(pair:Value)
                  XSharpToSystem:Add(pair:Value, pair:Key)
               ENDIF
            NEXT
            XSharpToSystem:Add("LONG", "System.Int32")

        STATIC METHOD GetSystemTypeName( SELF typename AS STRING, lXSharpNames AS LOGIC) AS STRING
            LOCAL lHandled := TRUE AS LOGIC
            IF lXSharpNames
                 SWITCH typename:ToLower()
                    CASE "array"
                        RETURN "XSharp.__Array"
                    CASE "date"
                        RETURN "XSharp.__Date"
                    CASE "float"
                        RETURN "XSharp.__Float"
                    CASE "psz"
                        RETURN "XSharp.__Psz"
                    CASE "symbol"
                        RETURN "XSharp.__Symbol"
                    CASE "usual"
                        RETURN "XSharp.__Usual"
                     OTHERWISE
                        lHandled := FALSE
                END SWITCH

             ELSE
                 SWITCH typename:ToLower()
                    CASE "array"
                        RETURN "Vulcan.__Array"
                    CASE "date"
                        RETURN "Vulcan.__VODate"
                    CASE "float"
                        RETURN "Vulcan.__VOFloat"
                    CASE "psz"
                        RETURN "Vulcan.__Psz"
                    CASE "symbol"
                        RETURN "Vulcan.__Symbol"
                    CASE "usual"
                        RETURN "Vulcan.__Usual"
                     OTHERWISE
                        lHandled := FALSE
                END SWITCH
            ENDIF
            IF ! lHandled .AND. XSharpToSystem:ContainsKey(typename)
                  typename := XSharpToSystem[typename]
            ENDIF
            RETURN typename


    STATIC METHOD GetXSharpTypeName( SELF typeName AS STRING) AS STRING
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
            ELSE
               IF (SystemToXSharp:ContainsKey(typeName))
                   typeName := SystemToXSharp[typeName]
               ENDIF
            ENDIF
            RETURN typeName

    STATIC METHOD GetXSharpTypeName( SELF sysType AS Mono.Cecil.TypeReference) AS STRING
            LOCAL fullName AS STRING
            LOCAL suffix AS STRING
            fullName := sysType:FullName
            IF (fullName == NULL)
                fullName := sysType:Name
            ENDIF
            suffix := ""
            IF fullName:EndsWith("[]")
                fullName := fullName:Substring(0, (fullName:Length - 2))
                suffix := "[]"
            ENDIF
            IF fullName:EndsWith("&")
                fullName := fullName:Substring(0, (fullName:Length - 1))
                suffix := ""
            ENDIF
            IF (SystemToXSharp:ContainsKey(fullName))
                //
                fullName := SystemToXSharp[fullName]
            ENDIF
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
            RETURN fullName+ suffix



    END CLASS

END NAMESPACE

