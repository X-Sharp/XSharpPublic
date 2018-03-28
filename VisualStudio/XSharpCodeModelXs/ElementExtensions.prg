//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
begin namespace XSharpModel
	
	static class ElementExtensions
		static method DisplayName( self elementKind as Kind) as string
			switch elementKind
				case Kind.VOGlobal
					return "GLOBAL"
				case Kind.VODefine
					return "DEFINE"
				case Kind.EnumMember
					return "MEMBER"
			end switch
			return elementKind:ToString():ToUpper()
		
		static method HasParameters( self elementKind as Kind) as logic
			switch elementKind
				case Kind.Constructor 
				case Kind.Method 
				case Kind.Assign 
				case Kind.Function 
				case Kind.Procedure 
				case Kind.Event 
				case Kind.Operator 
				case Kind.Delegate 
				case Kind.VODLL 
					//
					return true
			end switch
			return false
		
		static method HasReturnType( self elementKind as Kind) as logic
			switch elementKind
				case  Kind.Method 
				case Kind.Access 
				case Kind.Property 
				case Kind.Function 
				case Kind.Field 
				case Kind.Local 
				case Kind.Parameter 
				case Kind.Operator 
				case Kind.Delegate 
				case Kind.VOGlobal 
				case Kind.VODefine 
					return true
			end switch
			return false
		
		static method IsClassMember( self elementKind as Kind) as logic
			switch elementKind
				case Kind.Constructor 
				case Kind.Destructor 
				case Kind.Method 
				case Kind.Access 
				case Kind.Assign 
				case Kind.Property 
				case Kind.Event 
				case Kind.Operator 
					return true
			end switch
			return false
		
		static method IsField( self elementKind as Kind) as logic
			switch elementKind
				case  Kind.Field 
				case Kind.VOGlobal 
				case Kind.VODefine 
					return true
			end switch
			return false
		
		static method IsType( self elementKind as Kind) as logic
			switch elementKind
				case  Kind.Class 
				case Kind.Structure 
				case Kind.Interface 
				case Kind.Delegate 
				case Kind.Enum 
				case Kind.VOStruct 
				case Kind.Union 
					return true
			end switch
			return false
		
		
	end class
	
end namespace 

