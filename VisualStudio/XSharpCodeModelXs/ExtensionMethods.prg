//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
using System.Linq
using System.Collections.Generic
using System.Collections.Immutable
using LanguageService.CodeAnalysis.Text
begin namespace XSharpModel
	
	static class ExtensionMethods
		
		static method IsEmpty( self cType as CompletionType) as logic
			return cType == null .OR. ! cType:IsInitialized
		
		static method AddUnique<TKey, TValue>( self dict as Dictionary<TKey, TValue>, key as TKey, value as TValue) as TValue 
			if dict != null .AND. key != null
				if ! dict:ContainsKey(key)
					dict:Add(key, value)
					return value
				endif
				return dict:Item[key]
			endif
			return default (TValue)
		
		
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
				case Kind.Access
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
				case Kind.Method 
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
				case Kind.Field 
				case Kind.VOGlobal 
				CASE Kind.VODefine 
				case Kind.EnumMember
					return true
			end switch
			return false
		
		static method IsType( self elementKind as Kind) as logic
			switch elementKind
				case Kind.Class 
				case Kind.Structure 
				case Kind.Interface 
				case Kind.Delegate 
				case Kind.Enum 
				case Kind.VOStruct 
				case Kind.Union 
					return true
			end switch
			return false
		static method HasBody( self elementKind as Kind) as logic
			switch elementKind
				case Kind.Function
				case Kind.Procedure
				case Kind.Method
				case Kind.Access
				case Kind.Assign
				case Kind.Property
				case Kind.Event
				case Kind.Operator
				case Kind.Constructor
				case Kind.Destructor
					return true
			end switch
			return false
		
	// textspan extensions
		//static method GetText( self snapshot as ITextSnapshot, span as TextSpan) as string
			//return snapshot:GetText(Span{span:Start, span:Length})
		//
		//static method ToClassificationSpan( self span as TextSpan, snapshot as ITextSnapshot, classificationType as IClassificationType) as ClassificationSpan
			//return ClassificationSpan{SnapshotSpan{snapshot, span:Start, span:Length}, classificationType}
		//
		//static method ToTagSpan( self span as TextSpan, snapshot as ITextSnapshot, classificationType as IClassificationType) as ITagSpan<IClassificationTag>
			//return TagSpan<IClassificationTag>{SnapshotSpan{snapshot, span:Start, span:Length}, ClassificationTag{classificationType}}
	//
	//list exstensions
			static method AddUnique( self list as List<string>, item as string) as void
			if !list:Contains(item, System.StringComparer.OrdinalIgnoreCase)
				list:Add(item)
			endif
		
		static method Expanded( self source as IEnumerable<string>) as IReadOnlyList<string>
			local list as List<string>
			local item as string
			list := List<string>{}
			list:AddRange(source)
			foreach str as string in source
				item := str
				while (item:Contains("."))
					item := item:Substring(0, item:LastIndexOf("."))
					if (! list:Contains(item))
						list:Add(item)
					endif
				enddo
			next
			return List:ToImmutableList() 
		
		// parser enums
		static method ToModifiers(self mod as EntityModifiers) as Modifiers
		// FLags enum 
		local result as Modifiers
		if mod:HasFlag(EntityModifiers._Protected)
			result |= Modifiers.Protected
		endif
		if mod:HasFlag(EntityModifiers._Private)
			result |= Modifiers.Private
		endif
		if mod:HasFlag(EntityModifiers._Protected)
			result |= Modifiers.Protected
		endif
		if mod:HasFlag(EntityModifiers._Internal)
			result |= Modifiers.Internal
		endif
		if mod:HasFlag(EntityModifiers._Virtual)
			result |= Modifiers.Virtual
		endif
		if mod:HasFlag(EntityModifiers._Abstract)
			result |= Modifiers.Abstract
		endif
		if mod:HasFlag(EntityModifiers._Sealed)
			result |= Modifiers.Sealed
		endif
		if mod:HasFlag(EntityModifiers._Static)
			result |= Modifiers.Static
		endif
		if mod:HasFlag(EntityModifiers._Partial)
			result |= Modifiers.Partial
		endif
		if mod:HasFlag(EntityModifiers._New)
			result |= Modifiers.New
		endif
		if result == Modifiers.None
			result := Modifiers.Public
		endif
		return result

		static method ToModifiers(self acc as AccessLevel) as Modifiers
		// FLags enum 
		local result as Modifiers
		switch acc
		case AccessLevel.Hidden
			result := Modifiers.Hidden
		case AccessLevel.Protected
			result := Modifiers.Protected
		case AccessLevel.Public
			result := Modifiers.Public
		case AccessLevel.Internal
			result := Modifiers.Internal
		end switch
		if result == Modifiers.None
			result := Modifiers.Public
		endif

		return result


	end class
	
end namespace 


