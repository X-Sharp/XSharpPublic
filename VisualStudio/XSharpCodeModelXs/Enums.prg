//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
begin namespace XSharpModel
	internal enum ImageListKind as long
		member @@Block:=9
		member @@Class:=0
		member @@ClassMethod:=0x18
		member @@Const:=1
		member @@Delegate:=2
		member @@Enum:=3
		member @@EnumValue:=4
		member @@Event:=5
		member @@Field:=7
		member @@Interface:=8
		member @@Local:=0x17
		member @@Macro:=20
		member @@Method:=12
		member @@Namespace:=15
		member @@Operator:=0x10
		member @@Property:=0x11
		member @@StaticMethod:=13
		member @@Structure:=0x12
		member @@Unknown1:=6
		member @@Unknown11:=0x15
		member @@Unknown12:=0x16
		member @@Unknown6:=14
		member @@Unknown9:=0x13
		member @@Variant:=10
		member @@VariantOption:=11
	end enum
	
	internal enum ImageListOverlay as long
		member @@Internal:=1
		member @@ProtectedInternal:=2
		member @@Protected:=3
		member @@Private:=4
		member @@ImageListOverlayArrow:=5
		member @@Public:=0
	end enum
	
	enum Kind as long
		member @@Namespace
		member @@Class
		member @@Structure
		member @@Constructor
		member @@Destructor
		member @@Method
		member @@Access
		member @@Assign
		member @@Property
		member @@Function
		member @@Procedure
		member @@Field
		member @@Local
		member @@Parameter
		member @@Event
		member @@Operator
		member @@Interface
		member @@Delegate
		member @@Enum
		member @@EnumMember
		member @@Keyword
		member @@Union
		member @@Using
		member @@VODefine
		member @@VODLL
		member @@VOStruct
		member @@VOGlobal
	end enum
	enum Modifiers as long
		member @@Abstract:=0
		member @@New:=1
		member @@Partial:=2
		member @@Sealed:=3
		member @@Static:=4
		member @@Unsafe:=5
		member @@Hidden:=6
		member @@Private:=6
		member @@ProtectedInternal:=7
		member @@Internal:=8
		member @@Protected:=9
		member @@Public:=10
		member @@Export:=10
		member @@None:=11
	end enum
	enum XFileType as long
		member Unknown:=-1
		member SourceCode:=0
		member PreprocessorOutput:=1
		member Header:=2
		member VOForm:=3
		member VOMenu:=4
		member VODBServer:=5
		member VOIndex:=6
		member VOOrder:=7
		member VOFieldSpec:=8
		member NativeResource:=9
		member ManagedResource:=10
		member XAML:=11
		member Settings:=12
		member License:=13
	end enum
	
end namespace 

