//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Collections.Immutable
using System
begin namespace XSharpModel
	
	static class TypeExtensions
		// Fields
		static private lookupTable as IDictionary<string, string>
		
		// Methods
		static  constructor()
			super()
			//
			lookupTable  := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} 
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
			lookupTable := lookupTable:ToImmutableDictionary<string, string>(StringComparer.OrdinalIgnoreCase)
		
		static method GetSystemTypeName( self typename as string) as string
			//
			switch typename:ToLower()
				case "array"
					//
					return "Vulcan.__Array"
				case "date"
					//
					return "Vulcan.__VODate"
				case "float"
					//
					return "Vulcan.__VOFloat"
				case "psz"
					//
					return "Vulcan.__Psz"
				case "symbol"
					//
					return "Vulcan.__Symbol"
				case "usual"
					//
					return "Vulcan.__Usual"
			end switch
			return typename
		
		static method GetXSharpTypeName( self type as System.Type) as string
			local fullName as string
			local str2 as string
			//
			fullName := type:FullName
			if (fullName == null)
				//
				fullName := type:Name
			endif
			str2 := ""
			if (fullName:EndsWith("[]"))
				//
				fullName := fullName:Substring(0, (fullName:Length - 2))
				str2 := "[]"
			endif
			if (fullName:EndsWith("&"))
				//
				fullName := fullName:Substring(0, (fullName:Length - 1))
				str2 := ""
			endif
			if (lookupTable:ContainsKey(fullName))
				//
				fullName := lookupTable:Item[fullName]
			endif
			return String.Concat(fullName, str2)
		
		
	end class
	
end namespace 

