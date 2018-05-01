//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Collections.Immutable
USING System
BEGIN NAMESPACE XSharpModel
    
    STATIC CLASS TypeExtensions
        // Fields
        STATIC PRIVATE lookupTable AS IDictionary<STRING, STRING>
        
        // Methods
        STATIC  CONSTRUCTOR()
            SUPER()
            //
            lookupTable  := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase} 
            lookupTable:Add("System.Boolean", "LOGIC")
            lookupTable:Add("System.Byte", "BYTE")
            lookupTable:Add("System.String", "STRING")
            lookupTable:Add("System.Char", "CHAR")
            lookupTable:Add("System.Double", "REAL8")
            lookupTable:Add("System.Int16", "SHORT")
            lookupTable:Add("System.Int32", "INT")
            lookupTable:Add("System.Int64", "INT64")
            lookupTable:Add("System.Object", "OBJECT")
            lookupTable:Add("System.Single", "REAL4")
            lookupTable:Add("System.UInt16", "WORD")
            lookupTable:Add("System.UInt32", "DWORD")
            lookupTable:Add("System.UInt64", "UINT64")
            lookupTable:Add("System.Void", "VOID")
            lookupTable:Add("Vulcan._CodeBlock", "CODEBLOCK")
            lookupTable:Add("Vulcan.__Array", "UINT64")
            lookupTable:Add("Vulcan.__Psz", "PSZ")
            lookupTable:Add("Vulcan.__Symbol", "SYMBOL")
            lookupTable:Add("Vulcan.__Usual", "USUAL")
            lookupTable:Add("Vulcan.__VODate", "DATE")
            lookupTable:Add("Vulcan.__VOFloat", "FLOAT")
            lookupTable:Add("Vulcan.__WinBool", "LOGIC")
            lookupTable:Add("XSharp._CodeBlock", "CODEBLOCK")
            lookupTable:Add("XSharp.__Array", "UINT64")
            lookupTable:Add("XSharp.__Psz", "PSZ")
            lookupTable:Add("XSharp.__Symbol", "SYMBOL")
            lookupTable:Add("XSharp.__Usual", "USUAL")
            lookupTable:Add("XSharp.__VODate", "DATE")
            lookupTable:Add("XSharp.__VOFloat", "FLOAT")
            lookupTable:Add("XSharp.__WinBool", "LOGIC")
            lookupTable := lookupTable:ToImmutableDictionary<STRING, STRING>(StringComparer.OrdinalIgnoreCase)
            
        STATIC METHOD GetSystemTypeName( SELF typename AS STRING) AS STRING
            //
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
            END SWITCH
            RETURN typename
            
        STATIC METHOD GetXSharpTypeName( SELF type AS System.Type) AS STRING
            LOCAL fullName AS STRING
            LOCAL suffix AS STRING
            fullName := type:FullName
            IF (fullName == NULL)
                fullName := type:Name
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
            IF (lookupTable:ContainsKey(fullName))
                //
                fullName := lookupTable:Item[fullName]
            ENDIF
            //
            /*
            // Maybe it's a Raw format ?
            int genMarker = fullName:IndexOf('`')
            IF (genMarker > -1)
            // Class`1 -> Class
            var genTypeName = fullName:Substring(0, genMarker)
            local bFirst := TRUE as logic
            VAR genTemp = "<"
            VAR GenericTypeArguments = this.Type.GetGenericArguments();
            FOREACH (Type genArg IN GenericTypeArguments)
            {
            IF (bFirst)
                {
                genTemp += genArg.FullName;
                bFirst = FALSE;
            }
            ELSE
            genTemp += "," + genArg.FullName;
            }
            genTemp += ">";
            GenericTypeName = genTemp;
            }
            }
            //
            */
            //
            RETURN fullName+ suffix
            
            
    END CLASS
    
END NAMESPACE 

