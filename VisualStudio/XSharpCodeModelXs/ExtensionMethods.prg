//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Linq
USING System.Collections.Generic
USING System.Collections.Immutable
USING LanguageService.CodeAnalysis.Text
BEGIN NAMESPACE XSharpModel
	
	STATIC CLASS ExtensionMethods
		
		STATIC METHOD IsEmpty( SELF cType AS CompletionType) AS LOGIC
			RETURN cType == null .OR. ! cType:IsInitialized
		
		STATIC METHOD AddUnique<TKey, TValue>( SELF dict AS Dictionary<TKey, TValue>, key AS TKey, VALUE AS TValue) AS TValue 
			IF dict != null .AND. key != null
				IF ! dict:ContainsKey(key)
					dict:Add(key, VALUE)
					RETURN VALUE
				ENDIF
				RETURN dict:Item[key]
			ENDIF
			RETURN DEFAULT (TValue)
		
		
		STATIC METHOD DisplayName( SELF elementKind AS Kind) AS STRING
			SWITCH elementKind
				CASE Kind.VOGlobal
					RETURN "GLOBAL"
				CASE Kind.VODefine
					RETURN "DEFINE"
				CASE Kind.EnumMember
					RETURN "MEMBER"
			END SWITCH
			RETURN elementKind:ToString():ToUpper()
		
		STATIC METHOD HasParameters( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Constructor 
				CASE Kind.Method 
				CASE Kind.Assign 
				CASE Kind.Access
				CASE Kind.Function 
				CASE Kind.Procedure 
				CASE Kind.Event 
				CASE Kind.Operator 
				CASE Kind.Delegate 
				CASE Kind.VODLL 
					//
					RETURN true
			END SWITCH
			RETURN false
		
		STATIC METHOD HasReturnType( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Method 
				CASE Kind.Access 
				CASE Kind.Property 
				CASE Kind.Function 
				CASE Kind.Field 
				CASE Kind.Local 
				CASE Kind.Parameter 
				CASE Kind.Operator 
				CASE Kind.Delegate 
				CASE Kind.VOGlobal 
				CASE Kind.VODefine 
					RETURN true
			END SWITCH
			RETURN false
		
		STATIC METHOD IsClassMember( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Constructor 
				CASE Kind.Destructor 
				CASE Kind.Method 
				CASE Kind.Access 
				CASE Kind.Assign 
				CASE Kind.Property 
				CASE Kind.Event 
				CASE Kind.Operator 
					RETURN true
			END SWITCH
			RETURN false
		
		STATIC METHOD IsField( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Field 
				CASE Kind.VOGlobal 
				CASE Kind.VODefine 
				CASE Kind.EnumMember
					RETURN true
			END SWITCH
			RETURN false
		
		STATIC METHOD IsType( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Class 
				CASE Kind.Structure 
				CASE Kind.Interface 
				CASE Kind.Delegate 
				CASE Kind.Enum 
				CASE Kind.VOStruct 
				CASE Kind.Union 
					RETURN true
			END SWITCH
			RETURN false
		STATIC METHOD HasBody( SELF elementKind AS Kind) AS LOGIC
			SWITCH elementKind
				CASE Kind.Function
				CASE Kind.Procedure
				CASE Kind.Method
				CASE Kind.Access
				CASE Kind.Assign
				CASE Kind.Property
				CASE Kind.Event
				CASE Kind.Operator
				CASE Kind.Constructor
				CASE Kind.Destructor
					RETURN true
			END SWITCH
			RETURN false
		
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
			STATIC METHOD AddUnique( SELF list AS List<STRING>, item AS STRING) AS VOID
			IF !list:Contains(item, System.StringComparer.OrdinalIgnoreCase)
				list:Add(item)
			ENDIF
		
		STATIC METHOD Expanded( SELF source AS IEnumerable<STRING>) AS IReadOnlyList<STRING>
			LOCAL list AS List<STRING>
			LOCAL item AS STRING
			list := List<STRING>{}
			list:AddRange(source)
			FOREACH str AS STRING IN source
				item := str
				WHILE (item:Contains("."))
					item := item:Substring(0, item:LastIndexOf("."))
					IF (! list:Contains(item))
						list:Add(item)
					ENDIF
				ENDDO
			NEXT
			RETURN List:ToImmutableList() 
		
		// parser enums
		STATIC METHOD ToModifiers(SELF mod AS EntityModifiers) AS Modifiers
		// FLags enum 
		LOCAL result AS Modifiers
		IF mod:HasFlag(EntityModifiers._Protected)
			result |= Modifiers.Protected
		ENDIF
		IF mod:HasFlag(EntityModifiers._Private)
			result |= Modifiers.Private
		ENDIF
		IF mod:HasFlag(EntityModifiers._Protected)
			result |= Modifiers.Protected
		ENDIF
		IF mod:HasFlag(EntityModifiers._Internal)
			result |= Modifiers.Internal
		ENDIF
		IF mod:HasFlag(EntityModifiers._Virtual)
			result |= Modifiers.Virtual
		ENDIF
		IF mod:HasFlag(EntityModifiers._Abstract)
			result |= Modifiers.Abstract
		ENDIF
		IF mod:HasFlag(EntityModifiers._Sealed)
			result |= Modifiers.Sealed
		ENDIF
		IF mod:HasFlag(EntityModifiers._Static)
			result |= Modifiers.Static
		ENDIF
		IF mod:HasFlag(EntityModifiers._Partial)
			result |= Modifiers.Partial
		ENDIF
		IF mod:HasFlag(EntityModifiers._New)
			result |= Modifiers.New
		ENDIF
		IF result == Modifiers.None
			result := Modifiers.Public
		ENDIF
		RETURN result

		STATIC METHOD ToModifiers(SELF acc AS AccessLevel) AS Modifiers
		// FLags enum 
		LOCAL result AS Modifiers
		SWITCH acc
		CASE AccessLevel.Hidden
			result := Modifiers.Hidden
		CASE AccessLevel.Protected
			result := Modifiers.Protected
		CASE AccessLevel.Public
			result := Modifiers.Public
		CASE AccessLevel.Internal
			result := Modifiers.Internal
		END SWITCH
		IF result == Modifiers.None
			result := Modifiers.Public
		ENDIF

		RETURN result


	END CLASS
	
END NAMESPACE 


