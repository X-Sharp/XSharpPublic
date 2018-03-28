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
		
		// Methods
		constructor(name as string, kind as Kind, modifiers as Modifiers, visibility as Modifiers, range as TextRange, interval as TextInterval);super()
			
			self:_Name := name
			self:_Kind := kind
			self:_Modifiers := modifiers
			self:_Visibility := visibility
			self:_range := range
			self:_interval := interval
			self:_isStatic := false
		
		method ForceComplete() as void
			local parentName as string
			local fullName as string
			local type as XType
			
			if self:_parent == null .AND. ! String.IsNullOrEmpty(self:ParentName)
				
				parentName := self:ParentName
				fullName := self:FullName
				if ((parentName:IndexOf(".") == -1) .AND. (fullName:IndexOf(".") > 0))
					
					parentName := fullName:Substring(0, (fullName:LastIndexOf(".") + 1)) + parentName
				endif
				if self:file != null
					
					type := self:file:Project:LookupFullName(parentName, true)
					if type == null
						type := self:file:Project:Lookup(parentName, true)
					endif
					if type != null
						
						self:_parent := type
						self:_parent:ForceComplete()
					endif
				endif
			endif
		
		private method GetImageListIndex(kind as ImageListKind, overlay as ImageListOverlay) as long
			
			return (long)((kind * ImageListKind.Unknown1) + (ImageListKind)(long)overlay  ) 
		
		method OpenEditor() as void
			
			if self:_File != null .AND. self:_File:Project != null .AND. self:_File:Project:ProjectNode != null
				
				self:_File:Project:ProjectNode:OpenElement(self:_File:SourcePath, self:Range:StartLine, (self:Range:StartColumn + 1))
			endif
		
		
		// Properties
		virtual property Description as string
			get
				
				var desc := ""
				if (self:Modifiers != Modifiers.None)
					desc := desc + self:Modifiers:ToString()+ " "
				endif
				return desc + self:Visibility:ToString() +  " " + self:Kind:ToString() + " "+  self:Prototype
			end get
		end property
		
		property file as XFile get self:_File set self:_File := value
		
		property FileUsings as IImmutableList<string> get self:_File:Usings
		
		virtual property FullName as string get self:_Name
		
		property Glyph as long
			get
				
				var imgK := ImageListKind.Class
				var imgO := ImageListOverlay.Public
				switch self:Kind
					case Kind.Namespace
						
						imgK := ImageListKind.Namespace
					
					case Kind.Class
						
						imgK := ImageListKind.Class
					
					case Kind.Structure
					case Kind.VOStruct 
					case Kind.Union 
						
						imgK := ImageListKind.Structure
					
					case  Kind.Constructor 
					case Kind.Destructor 
					case Kind.Method 
					case Kind.Function 
					case Kind.Procedure 
						
						imgK := ImageListKind.Method
					
					case Kind.Access 
					case Kind.Assign 
					case Kind.Property 
						
						imgK := ImageListKind.Property
					
					case  Kind.Field 
					case Kind.VOGlobal 
						
						imgK := ImageListKind.Field
					
					case  Kind.Local 
					case Kind.Parameter 
						
						imgK := ImageListKind.Local
					
					case Kind.Event
						
						imgK := ImageListKind.Event
					
					case Kind.Operator
						
						imgK := ImageListKind.Operator
					
					case Kind.Interface
						
						imgK := ImageListKind.Interface
					
					case Kind.Delegate
						
						imgK := ImageListKind.Delegate
					
					case Kind.Enum
						
						imgK := ImageListKind.Enum
					
					case Kind.VODefine
						
						imgK := ImageListKind.Const
					
					case Kind.EnumMember
						
						imgK := ImageListKind.EnumValue
					
				end switch
				switch self:Visibility
					case Modifiers.Private
						
						imgO := ImageListOverlay.Private
					
					case Modifiers.ProtectedInternal
						
						imgO := ImageListOverlay.ProtectedInternal
					
					case Modifiers.Internal
						
						imgO := ImageListOverlay.Internal
					
					case Modifiers.Protected
						
						imgO := ImageListOverlay.Protected
					
					case Modifiers.Public
						
						imgO := ImageListOverlay.Public
					
				end switch
				return self:GetImageListIndex(imgK, imgO)
			end get
		end property
		
		property GlyphGroup as StandardGlyphGroup
			get
				var imgG := StandardGlyphGroup.GlyphGroupClass
				switch self:Kind
					case Kind.Namespace
						return StandardGlyphGroup.GlyphGroupNamespace
					case Kind.Class
						return StandardGlyphGroup.GlyphGroupClass
					case Kind.Structure
						
						return StandardGlyphGroup.GlyphGroupStruct
					case  Kind.Constructor 
					case Kind.Destructor 
					case Kind.Method 
					case Kind.Function 
					case Kind.Procedure 
						
						return StandardGlyphGroup.GlyphGroupMethod
					case Kind.Access 
					case Kind.Assign 
					case Kind.Property 
						
						return StandardGlyphGroup.GlyphGroupProperty
					case  Kind.Field 
					case Kind.VOGlobal 
						
						return StandardGlyphGroup.GlyphGroupField
					case  Kind.Local 
					case Kind.Parameter 
						
						return StandardGlyphGroup.GlyphGroupVariable
					case Kind.Event
						
						return StandardGlyphGroup.GlyphGroupEvent
					case Kind.Operator
						
						return StandardGlyphGroup.GlyphGroupOperator
					case Kind.Interface
						
						return StandardGlyphGroup.GlyphGroupInterface
					case Kind.Delegate
						
						return StandardGlyphGroup.GlyphGroupDelegate
					case Kind.Enum
						
						return StandardGlyphGroup.GlyphGroupEnum
					case Kind.Using 
					case Kind.VODLL 
						return imgG
					case Kind.VODefine
						
						return StandardGlyphGroup.GlyphGroupConstant
					case Kind.VOStruct
						
						return StandardGlyphGroup.GlyphGroupValueType
					case Kind.Union
						
						return StandardGlyphGroup.GlyphGroupUnion
					case Kind.EnumMember
						
						return StandardGlyphGroup.GlyphGroupEnumMember
					case Kind.Keyword
						
						return StandardGlyphGroup.GlyphKeyword
				end switch
				return imgG
			end get
		end property
		
		property GlyphItem as StandardGlyphItem
			get
				
				var imG := StandardGlyphItem.GlyphItemPublic
				switch self:Visibility
					case Modifiers.Private
						
						imG := StandardGlyphItem.GlyphItemPrivate
					
					case Modifiers.ProtectedInternal
						
						imG := StandardGlyphItem.GlyphItemProtected
					
					case Modifiers.Internal
						
						imG := StandardGlyphItem.GlyphItemInternal
					
					case Modifiers.Protected
						
						imG := StandardGlyphItem.GlyphItemProtected
					
					case Modifiers.Public
						
						imG := StandardGlyphItem.GlyphItemPublic
					
				end switch
				if self:IsStatic
					imG := StandardGlyphItem.GlyphItemShortcut
				endif
				return imG
			end get
		end property
		
		property Interval as TextInterval get self:_interval
		property IsStatic as logic get _isStatic set _isStatic := value
		property Kind as Kind get self:_Kind
		property Language as string get "XSharp"
		property Modifiers as Modifiers GET self:_Modifiers
		property Name as string get self:_Name
		property Parent as XElement GET self:_parent SET self:_parent := value
		virtual property ParentName as string get self:_parent?:FullName
		
		property ProjectItem as ProjectItem
			get
				throw System.NotImplementedException{}
			end get
		end property
		
		property Prototype as string get self:Name
		property Range as TextRange get self:_range
		property Visibility as Modifiers get self:_Visibility
		
		
	end class
	
end namespace 

