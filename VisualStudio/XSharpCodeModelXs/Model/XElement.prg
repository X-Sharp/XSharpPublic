//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING Microsoft.VisualStudio.Language.Intellisense
USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq

BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("Kind, {Name,nq}")];
	CLASS XElement
		// Fields
		PRIVATE _File AS XFile
		PRIVATE _interval AS TextInterval
		PROTECTED _isStatic AS LOGIC
		PRIVATE _Kind AS Kind
		PRIVATE _Modifiers AS Modifiers
		PRIVATE _Name AS STRING
		PRIVATE _parent AS XElement
		PRIVATE _range AS TextRange
		PRIVATE _Visibility AS Modifiers
		STATIC PRIVATE _nKeywordCase := -1 AS INT
		CONST PUBLIC GlobalName := "(Global Scope)" AS STRING
		
		// Methods
		CONSTRUCTOR()
			RETURN
			
		CONSTRUCTOR(name AS STRING, kind AS Kind, modifiers AS Modifiers, visibility AS Modifiers, range AS TextRange, interval AS TextInterval)
			SUPER()
			SELF:_Name := name
			SELF:_Kind := kind
			SELF:_Modifiers := modifiers
			SELF:_Visibility := visibility
			SELF:_range := range
			SELF:_interval := interval
			SELF:_isStatic := FALSE
			
			#region Simple Properties
			PROPERTY File AS XFile GET SELF:_File SET SELF:_File := VALUE
			VIRTUAL PROPERTY FullName AS STRING GET SELF:_Name
			PROPERTY Kind AS Kind GET SELF:_Kind
			PROPERTY Language AS STRING GET "XSharp"
			PROPERTY Modifiers AS Modifiers GET SELF:_Modifiers
			PROPERTY Name AS STRING GET SELF:_Name
			PROPERTY Interval AS TextInterval GET SELF:_interval
			PROPERTY IsStatic AS LOGIC GET _isStatic SET _isStatic := VALUE
			PROPERTY FileUsings AS IList<STRING> GET SELF:_File:Usings
			PROPERTY Parent AS XElement GET SELF:_parent SET SELF:_parent := VALUE
			VIRTUAL PROPERTY ParentName AS STRING GET SELF:_parent?:FullName
			VIRTUAL PROPERTY Prototype AS STRING GET SELF:Name
			VIRTUAL PROPERTY ComboPrototype AS STRING GET SELF:Name
			PROPERTY Range AS TextRange GET SELF:_range
			PROPERTY Visibility AS Modifiers GET SELF:_Visibility
			
			
		#endregion
		
		METHOD ForceComplete() AS VOID
			LOCAL parentName AS STRING
			LOCAL thisName AS STRING
			LOCAL tmp AS XType
			
			IF SELF:_parent == NULL .AND. ! String.IsNullOrEmpty(SELF:ParentName)
			
				parentName := SELF:ParentName
				thisName := SELF:FullName
				IF parentName:IndexOf(".") == -1 .AND. thisName:IndexOf(".") > 0
				
					parentName := thisName:Substring(0, (thisName:LastIndexOf(".") + 1)) + parentName
				ENDIF
				IF SELF:File != NULL .and. parentName != "System.Object"
				
					tmp := SELF:File:Project:Lookup(parentName, TRUE)
					IF tmp != NULL
					
						SELF:_parent := tmp
						// Ensure whole tree is resolved.
						SELF:_parent:ForceComplete()
					ENDIF
				ENDIF
			ENDIF
			
		PRIVATE METHOD GetImageListIndex(kind AS ImageListKind, overlay AS ImageListOverlay) AS LONG
		
			RETURN (LONG)((kind * ImageListKind.Unknown1) + (ImageListKind)(LONG)overlay  ) 
			
		METHOD OpenEditor() AS VOID
			IF SELF:_File?:Project?:ProjectNode != NULL
				SELF:_File:Project:ProjectNode:OpenElement(SELF:_File:SourcePath, SELF:Range:StartLine, (SELF:Range:StartColumn + 1))
			ENDIF
			
		STATIC METHOD Etype2Kind(eType AS EntityType) AS Kind
			SWITCH eType
				CASE EntityType._Constructor
					RETURN Kind.Constructor
				CASE EntityType._Destructor
					RETURN Kind.Destructor
				CASE EntityType._Method
					RETURN Kind.Method
				CASE EntityType._Access
					RETURN Kind.Access
				CASE EntityType._Assign
					RETURN Kind.Assign
				CASE EntityType._Class
					RETURN Kind.CLASS
				CASE EntityType._Function
					RETURN Kind.Function
				CASE EntityType._Procedure
					RETURN Kind.Procedure
				CASE EntityType._Enum
					RETURN Kind.Enum
				CASE EntityType._EnumMember
					RETURN Kind.EnumMember
				CASE EntityType._VOStruct
					RETURN Kind.VOStruct
				CASE EntityType._Global
					RETURN Kind.VOGLobal
				CASE EntityType._Structure
					RETURN Kind.Structure
				CASE EntityType._Interface
					RETURN Kind.Interface
				CASE EntityType._Delegate
					RETURN Kind.Delegate
				CASE EntityType._Event
					RETURN Kind.Event
				CASE EntityType._Field
					RETURN Kind.Field
				CASE EntityType._Union
					RETURN Kind.Union
				CASE EntityType._Operator
					RETURN Kind.Operator
				CASE EntityType._Local
					RETURN Kind.Local
				CASE EntityType._Property
					RETURN Kind.Property
				CASE EntityType._Define
					RETURN Kind.VODefine
				CASE EntityType._Resource
				CASE EntityType._TextBlock
				OTHERWISE
					RETURN Kind.Unknown
			END SWITCH
			
		STATIC METHOD CalculateRange(oElement AS EntityObject, oInfo AS ParseResult, span OUT TextRange, interval OUT TextInterval) AS VOID
			LOCAL nLineStart, nColStart AS INT	
			LOCAL nLineEnd, nColEnd		AS INT
			LOCAL nPosStart, nPosEnd AS LONG
			LOCAL lHasEnd AS LOGIC
			nLineStart := oElement:nStartLine  // parser has 1 based lines and columns
			nLineEnd   := oElement:nStartLine
			nColStart  := oElement:nCol
			nColEnd	   := 1 
			nPosStart  := oElement:nOffSet
			lHasEnd    := FALSE
			IF oElement:cName == GlobalName
				nLineEnd   := nLineStart		
				nColEnd    := nColStart
				lHasEnd    := TRUE
			ELSEIF oElement:eType:IsType()
				// find the last member
				// and the entity after it is our end
				VAR oLast := oElement:aChildren:LastOrDefault()
				IF oLast != NULL_OBJECT 
					IF oLast:oNext != NULL_OBJECT 
						nLineEnd := oLast:oNext:nStartLine -1
						nPosEnd  := oLast:oNext:nOffSet -2		// subtract CRLF
						lHasEnd    := TRUE
					ENDIF
				ELSE
					// type without children ?
					IF oElement:oNext != NULL_OBJECT 
						nLineEnd := oElement:oNext:nStartLine -1
						nPosEnd  := oElement:oNext:nOffSet -2		// subtract CRLF
						lHasEnd    := TRUE
					ENDIF
				ENDIF
			ELSEIF oElement:oNext != NULL_OBJECT 
				nLineEnd := oElement:oNext:nStartLine -1
				nPosEnd  := oElement:oNext:nOffSet -2		// subtract CRLF
				lHasEnd    := TRUE
			ENDIF
			IF ! lHasEnd
				nLineEnd   := oInfo:LineCount		
				nColEnd    := nColStart
				nPosEnd    := oInfo:SourceLength-2
			ENDIF
			IF nLineStart == nLineEnd .and. nColEnd < nColStart
				nColEnd := nColStart + 1
			ENDIF
			span	 := TextRange{nLineStart, nColStart, nLineEnd, nColEnd}
			interval := TextInterval{nPosStart, nPosEnd}
			RETURN
			
			#region Complexer properties		
			// Properties
			VIRTUAL PROPERTY Description AS STRING
				GET
					VAR modVis := ""
					IF SELF:Modifiers != Modifiers.None
						modVis := modVis + SELF:ModifiersKeyword
					ENDIF
					modVis += VisibilityKeyword
					VAR desc := modVis
					desc += KindKeyword +  SELF:Prototype
					RETURN desc
				END GET
			END PROPERTY
			
			
			
			/// <summary>
			/// Glyph constant used by DropDown Types/Members Comboxes in Editor
			/// </summary>
			PROPERTY Glyph AS LONG
				GET
				
					VAR imgK := ImageListKind.Class
					VAR imgO := ImageListOverlay.Public
					SWITCH SELF:Kind
						CASE Kind.Class
							imgK := ImageListKind.Class
						CASE  Kind.Constructor 
						CASE Kind.Destructor 
						CASE Kind.Method 
						CASE Kind.Function 
						CASE Kind.Procedure 
							imgK := ImageListKind.Method
					CASE Kind.Structure
						CASE Kind.VOStruct 
						CASE Kind.Union 
							imgK := ImageListKind.Structure
						CASE Kind.Access 
						CASE Kind.Assign 
						CASE Kind.Property 
							imgK := ImageListKind.Property
						CASE Kind.Event
							imgK := ImageListKind.Event
						CASE Kind.Delegate
						
							imgK := ImageListKind.Delegate
						CASE Kind.Operator
							imgK := ImageListKind.Operator
						CASE Kind.VODefine
							imgK := ImageListKind.Const
						CASE Kind.Enum
							imgK := ImageListKind.Enum
						CASE Kind.EnumMember
						
							imgK := ImageListKind.EnumValue
						CASE Kind.Interface
							imgK := ImageListKind.Interface
						CASE Kind.Namespace
							imgK := ImageListKind.Namespace
						CASE Kind.VOGlobal 
						CASE Kind.Field 
							imgK := ImageListKind.Field
						CASE Kind.Parameter 
						CASE Kind.Local 
							imgK := ImageListKind.Local
						END SWITCH
					SWITCH SELF:Visibility
						CASE Modifiers.Public
							imgO := ImageListOverlay.Public
						CASE Modifiers.Protected
							imgO := ImageListOverlay.Protected
						CASE Modifiers.Private
							imgO := ImageListOverlay.Private
						CASE Modifiers.Internal
							imgO := ImageListOverlay.Internal
						CASE Modifiers.ProtectedInternal
							imgO := ImageListOverlay.ProtectedInternal
							
					END SWITCH
					RETURN SELF:GetImageListIndex(imgK, imgO)
				END GET
			END PROPERTY
			
			PROPERTY GlyphGroup AS StandardGlyphGroup
				GET
					LOCAL imgG := StandardGlyphGroup.GlyphGroupClass AS StandardGlyphGroup 
					SWITCH SELF:Kind
						CASE Kind.Class
							imgG :=  StandardGlyphGroup.GlyphGroupClass
						CASE  Kind.Constructor 
						CASE Kind.Destructor 
						CASE Kind.Method 
					CASE Kind.Function 
						CASE Kind.Procedure 
							imgG :=  StandardGlyphGroup.GlyphGroupMethod
						CASE Kind.Structure
							imgG :=  StandardGlyphGroup.GlyphGroupStruct
						CASE Kind.Access 
						CASE Kind.Assign 
						CASE Kind.Property 
							imgG :=  StandardGlyphGroup.GlyphGroupProperty
						CASE Kind.Parameter 
						CASE Kind.Local 
							imgG :=  StandardGlyphGroup.GlyphGroupVariable
						CASE Kind.Event
							imgG :=  StandardGlyphGroup.GlyphGroupEvent
						CASE Kind.Delegate
							imgG :=  StandardGlyphGroup.GlyphGroupDelegate
						CASE Kind.Enum
							imgG :=  StandardGlyphGroup.GlyphGroupEnum
						CASE Kind.EnumMember
							imgG :=  StandardGlyphGroup.GlyphGroupEnumMember
						CASE Kind.Operator
							imgG :=  StandardGlyphGroup.GlyphGroupOperator
						CASE Kind.Interface
							imgG :=  StandardGlyphGroup.GlyphGroupInterface
						CASE Kind.Namespace
							imgG :=  StandardGlyphGroup.GlyphGroupNamespace
						CASE Kind.Field 
						CASE Kind.VOGlobal 
							imgG :=  StandardGlyphGroup.GlyphGroupField
						CASE Kind.Union
							imgG :=  StandardGlyphGroup.GlyphGroupUnion
						CASE Kind.VODefine
							imgG :=  StandardGlyphGroup.GlyphGroupConstant
						CASE Kind.VOStruct
							imgG :=  StandardGlyphGroup.GlyphGroupValueType
						CASE Kind.Keyword
							imgG :=  StandardGlyphGroup.GlyphKeyword
						END SWITCH
					RETURN imgG
				END GET
			END PROPERTY
			
			PROPERTY GlyphItem AS StandardGlyphItem
				GET
				
					LOCAL imgI := StandardGlyphItem.GlyphItemPublic AS StandardGlyphItem
					SWITCH SELF:Visibility
						CASE Modifiers.Public
							imgI := StandardGlyphItem.GlyphItemPublic
						CASE Modifiers.Protected
							imgI := StandardGlyphItem.GlyphItemProtected
						CASE Modifiers.Private
							imgI := StandardGlyphItem.GlyphItemPrivate
						CASE Modifiers.Internal
							imgI := StandardGlyphItem.GlyphItemInternal
						CASE Modifiers.ProtectedInternal
							imgI := StandardGlyphItem.GlyphItemProtected
					END SWITCH
					IF SELF:IsStatic
						imgI := StandardGlyphItem.GlyphItemShortcut
					ENDIF
					RETURN imgI
				END GET
			END PROPERTY
			
			
//			PROPERTY ProjectItem AS ProjectItem
//				GET
//					THROW System.NotImplementedException{}
//				END GET
//			END PROPERTY
			
			PROPERTY KeywordsUpperCase AS LOGIC
				GET 
					IF _nKeywordCase == -1
						IF SELF:File?:Project?:ProjectNode != NULL
							_nKeywordcase := IIF(SELF:File:Project:ProjectNode:KeywordsUppercase,1,0)
						ELSE
							_nKeywordcase := 1
						ENDIF
					ENDIF
					RETURN _nKeywordCase == 1
				END GET
			END PROPERTY
			
			STATIC PROPERTY AsKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " AS ", " as ")
			STATIC PROPERTY RefKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " REF ", " ref ")
			STATIC PROPERTY OutKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " OUT ", " out ")
			STATIC PROPERTY ParamsKeyWord		AS STRING GET IIF(_nKeywordCase == 1, " PARAMS ", " params ")
			PROPERTY ModifiersKeyword			AS STRING GET IIF(KeywordsUpperCase, SELF:Modifiers:ToString():ToUpper(), SELF:Modifiers:ToString():ToLower()) + " "
			PROPERTY VisibilityKeyword			AS STRING GET IIF(KeywordsUpperCase, SELF:Visibility:ToString():ToUpper(), SELF:Visibility:ToString():ToLower()) + " "
			PROPERTY KindKeyword				AS STRING GET IIF(KeywordsUpperCase, SELF:Kind:DisplayName():ToUpper(), SELF:Kind:DisplayName():ToLower()) + " "
			
			
			#endregion
	END CLASS
	
END NAMESPACE 


