//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
begin namespace XSharpModel
	internal enum ImageListKind as Int32
		member @@Class:=0
		member @@Const:=1
		member @@Delegate:=2
		member @@Enum:=3
		member @@EnumValue:=4
		member @@Event:=5
		member @@Unknown1:=6
		member @@Field:=7
		member @@Interface:=8
		member @@Block:=9
		member @@Variant:=10
		member @@VariantOption:=11
		member @@Method:=12
		member @@StaticMethod:=13
		member @@Unknown6:=14
		member @@Namespace:=15
		member @@Operator:=16
		member @@Property:=17
		member @@Structure:=18
		member @@Unknown9:=19
		member @@Macro:=20
		member @@Unknown11:=21
		member @@Unknown12:=22
		member @@Local:=23
		member @@ClassMethod:=24
	end enum
	
	internal enum ImageListOverlay as Int32
		member @@Internal:=1
		member @@ProtectedInternal:=2
		member @@Protected:=3
		member @@Private:=4
		member @@ImageListOverlayArrow:=5
		member @@Public:=0
	end enum
	
	enum Kind as Int32 
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
		member @@Unknown
	END ENUM

	[Flags];
	enum Modifiers as long
		member @@None:=0
		member @@Private:= 0x01
		member @@Hidden:=  0x01
		member @@ProtectedInternal:=0x02
		member @@Internal:=0x04
		member @@Protected:=0x08
		member @@Public:=0x10
		member @@Export:=0x10
		member @@VisibilityMask := 0xFF
		member @@Abstract:=0x100
		member @@New:=0x200
		member @@Partial:=0x400
		member @@Sealed:=0x800
		member @@Static:=0x1000
		member @@Unsafe:=0x2000
		member @@Virtual:=0x4000
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
	
	ENUM ParamType AS BYTE
		MEMBER @@AS		:= 0
		MEMBER @@Ref    := 1
		MEMBER @@Out	:= 2
		Member @@Params := 3
	end enum
end namespace 

