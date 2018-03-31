//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using EnvDTE
using Microsoft.VisualStudio.Language.Intellisense

using System.Collections.Generic
using System.Collections.Immutable
using System.Diagnostics
using System
using System.Linq
begin namespace XSharpModel
	[DebuggerDisplay("Kind, {Name,nq}")];
	class XElement
		// Fields
		private _File as XFile
		private _interval as TextInterval
		protected _isStatic as logic
		private _Kind as Kind
		private _Modifiers as Modifiers
		private _Name as string
		private _parent as XElement
		private _range as TextRange
		private _Visibility as Modifiers
		const public GlobalName := "(Global Scope)" as string

		// Methods
		constructor()
			return
		
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, range as TextRange, interval as TextInterval)
			super()
			self:_Name := name
			self:_Kind := kind
			self:_Modifiers := modifiers
			self:_Visibility := visibility
			self:_range := range
			self:_interval := interval
			self:_isStatic := false
		
		#region Simple Properties
			property File as XFile get self:_File set self:_File := value
			property FullName as string get self:_Name
			property Kind as Kind get self:_Kind
			property Language as string get "XSharp"
			property Modifiers as Modifiers get self:_Modifiers
			property Name as string get self:_Name
			property Interval as TextInterval get self:_interval
			property IsStatic as logic get _isStatic set _isStatic := value
			property FileUsings as IImmutableList<string> get self:_File:Usings
			property Parent as XElement get self:_parent set self:_parent := value
			property ParentName as string get self:_parent?:FullName
			property Prototype as string get self:Name
			property Range as TextRange get self:_range
			property Visibility as Modifiers get self:_Visibility
			
			
		#endregion
		
		method ForceComplete() as void
			local parentName as string
			local thisName as string
			local tmp as XType
			
			if self:_parent == null .AND. ! String.IsNullOrEmpty(self:ParentName)
				
				parentName := self:ParentName
				thisName := self:FullName
				if ((parentName:IndexOf(".") == -1) .AND. (thisName:IndexOf(".") > 0))
					
					parentName := thisName:Substring(0, (thisName:LastIndexOf(".") + 1)) + parentName
				endif
				if self:File != null
					
					tmp := self:File:Project:LookupFullName(parentName, true)
					if tmp == null
						tmp := self:File:Project:Lookup(parentName, true)
					endif
					if tmp != null
						
						self:_parent := tmp
						// Ensure whole tree is resolved.
						self:_parent:ForceComplete()
					endif
				endif
			endif
		
		private method GetImageListIndex(kind as ImageListKind, overlay as ImageListOverlay) as long
			
			return (long)((kind * ImageListKind.Unknown1) + (ImageListKind)(long)overlay  ) 
		
		method OpenEditor() as void
			if self:_File?:Project?:ProjectNode != null
				self:_File:Project:ProjectNode:OpenElement(self:_File:SourcePath, self:Range:StartLine, (self:Range:StartColumn + 1))
			endif
		
		static method Etype2Kind(eType as EntityType) as Kind
			switch eType
				case EntityType._Constructor
					return Kind.Constructor
				case EntityType._Destructor
					return Kind.Destructor
				case EntityType._Method
					return Kind.Method
				case EntityType._Access
					return Kind.Access
				case EntityType._Assign
					return Kind.Assign
				case EntityType._Class
					return Kind.CLASS
				case EntityType._Function
					return Kind.Function
				case EntityType._Procedure
					return Kind.Procedure
				case EntityType._Enum
					return Kind.Enum
				case EntityType._VOStruct
					return Kind.VOStruct
				case EntityType._Global
					return Kind.VOGLobal
				case EntityType._Structure
					return Kind.Structure
				case EntityType._Interface
					return Kind.Interface
				case EntityType._Delegate
					return Kind.Delegate
				case EntityType._Event
					return Kind.Event
				case EntityType._Field
					return Kind.Field
				case EntityType._Union
					return Kind.Union
				case EntityType._Operator
					return Kind.Operator
				case EntityType._Local
					return Kind.Local
				case EntityType._Property
					return Kind.Property
				case EntityType._Define
					return Kind.VODefine
				case EntityType._Resource
				case EntityType._TextBlock
				otherwise
					return Kind.Unknown
			end switch
		
		static method CalculateRange(oElement as EntityObject, oInfo as ParseResult, span out TextRange, interval out TextInterval) as void
			local nLineStart, nColStart as int	
			local nLineEnd, nColEnd		as int
			local oLast					as EntityObject
			local nPosStart, nPosEnd as long
			nLineStart := oElement:nStartLine  // parser has 1 based lines and columns
			nLineEnd   := oElement:nStartLine
			nColStart  := oElement:nCol
			nPosStart  := oElement:nOffSet
			if oElement:cName == GlobalName
				nLineEnd   := nLineStart		
				nColEnd    := nColStart
			elseif oElement:oNext != NULL_OBJECT
				nLineEnd := oElement:oNext:nStartLine -1
				nPosEnd  := oElement:oNext:nOffSet -2		// subtract CRLF
			else
				nLineEnd   := nLineStart		
				nColEnd    := nColStart
				nPosEnd    := oInfo:SourceLength-2
			endif
			oLast := oElement
			span	 := TextRange{nLineStart, nColStart, nLineEnd, nColEnd}
			interval := TextInterval{nPosStart, nPosEnd}
			return
		
		#region Complexer properties		
			// Properties
			property Description as string
				get
					var modVis := ""
					if (self:Modifiers != Modifiers.None)
						modVis := modVis + self:Modifiers:ToString()+ " "
					endif
					modVis += self:Visibility:ToString()
					var desc := modVis
					desc += self:Kind:ToString() 
					desc += " "+  self:Prototype
					return desc
				end get
			end property
			
			
			
			/// <summary>
			/// Glyph constant used by DropDown Types/Members Comboxes in Editor
			/// </summary>
			property Glyph as long
				get
					
					var imgK := ImageListKind.Class
					var imgO := ImageListOverlay.Public
					switch self:Kind
						case Kind.Class
							imgK := ImageListKind.Class
						case  Kind.Constructor 
						case Kind.Destructor 
						case Kind.Method 
						case Kind.Function 
						case Kind.Procedure 
							imgK := ImageListKind.Method
						case Kind.Structure
						case Kind.VOStruct 
						case Kind.Union 
							imgK := ImageListKind.Structure
						case Kind.Access 
						case Kind.Assign 
						case Kind.Property 
							imgK := ImageListKind.Property
						case Kind.Event
							imgK := ImageListKind.Event
						case Kind.Delegate
							
							imgK := ImageListKind.Delegate
						case Kind.Operator
							imgK := ImageListKind.Operator
						case Kind.VODefine
							imgK := ImageListKind.Const
						case Kind.Enum
							imgK := ImageListKind.Enum
						case Kind.EnumMember
							
							imgK := ImageListKind.EnumValue
						case Kind.Interface
							imgK := ImageListKind.Interface
						case Kind.Namespace
							imgK := ImageListKind.Namespace
						case Kind.VOGlobal 
						case Kind.Field 
							imgK := ImageListKind.Field
						case Kind.Parameter 
						case Kind.Local 
							imgK := ImageListKind.Local
					end switch
					switch self:Visibility
						case Modifiers.Public
							imgO := ImageListOverlay.Public
						case Modifiers.Protected
							imgO := ImageListOverlay.Protected
						case Modifiers.Private
							imgO := ImageListOverlay.Private
						case Modifiers.Internal
							imgO := ImageListOverlay.Internal
						case Modifiers.ProtectedInternal
							imgO := ImageListOverlay.ProtectedInternal
						
					end switch
					return self:GetImageListIndex(imgK, imgO)
				end get
			end property
			
			property GlyphGroup as StandardGlyphGroup
				get
					local imgG := StandardGlyphGroup.GlyphGroupClass as StandardGlyphGroup 
					switch self:Kind
						case Kind.Class
							imgG :=  StandardGlyphGroup.GlyphGroupClass
						case  Kind.Constructor 
						case Kind.Destructor 
						case Kind.Method 
						case Kind.Function 
						case Kind.Procedure 
							imgG :=  StandardGlyphGroup.GlyphGroupMethod
						case Kind.Structure
							imgG :=  StandardGlyphGroup.GlyphGroupStruct
						case Kind.Access 
						case Kind.Assign 
						case Kind.Property 
							imgG :=  StandardGlyphGroup.GlyphGroupProperty
						case Kind.Parameter 
						case Kind.Local 
							imgG :=  StandardGlyphGroup.GlyphGroupVariable
						case Kind.Event
							imgG :=  StandardGlyphGroup.GlyphGroupEvent
						case Kind.Delegate
							imgG :=  StandardGlyphGroup.GlyphGroupDelegate
						case Kind.Enum
							imgG :=  StandardGlyphGroup.GlyphGroupEnum
						case Kind.EnumMember
							imgG :=  StandardGlyphGroup.GlyphGroupEnumMember
						case Kind.Operator
							imgG :=  StandardGlyphGroup.GlyphGroupOperator
						case Kind.Interface
							imgG :=  StandardGlyphGroup.GlyphGroupInterface
						case Kind.Namespace
							imgG :=  StandardGlyphGroup.GlyphGroupNamespace
						case Kind.Field 
						case Kind.VOGlobal 
							imgG :=  StandardGlyphGroup.GlyphGroupField
						case Kind.Union
							imgG :=  StandardGlyphGroup.GlyphGroupUnion
						case Kind.VODefine
							imgG :=  StandardGlyphGroup.GlyphGroupConstant
						case Kind.VOStruct
							imgG :=  StandardGlyphGroup.GlyphGroupValueType
						case Kind.Keyword
							imgG :=  StandardGlyphGroup.GlyphKeyword
					end switch
					return imgG
				end get
			end property
			
			property GlyphItem as StandardGlyphItem
				get
					
					local imgI := StandardGlyphItem.GlyphItemPublic as StandardGlyphItem
					switch self:Visibility
						case Modifiers.Public
							imgI := StandardGlyphItem.GlyphItemPublic
						case Modifiers.Protected
							imgI := StandardGlyphItem.GlyphItemProtected
						case Modifiers.Private
							imgI := StandardGlyphItem.GlyphItemPrivate
						case Modifiers.Internal
							imgI := StandardGlyphItem.GlyphItemInternal
						case Modifiers.ProtectedInternal
							imgI := StandardGlyphItem.GlyphItemProtected
					end switch
					if self:IsStatic
						imgI := StandardGlyphItem.GlyphItemShortcut
					endif
					return imgI
				end get
			end property
			
			
			property ProjectItem as ProjectItem
				get
					throw System.NotImplementedException{}
				end get
			end property
		#endregion
	end class
	
end namespace 


