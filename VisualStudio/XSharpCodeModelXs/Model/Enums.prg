//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
BEGIN NAMESPACE XSharpModel
	INTERNAL ENUM ImageListKind AS Int32
		MEMBER @@Class:=0
		MEMBER @@Const:=1
		MEMBER @@Delegate:=2
		MEMBER @@Enum:=3
		MEMBER @@EnumValue:=4
		MEMBER @@Event:=5
		MEMBER @@Unknown1:=6
		MEMBER @@Field:=7
		MEMBER @@Interface:=8
		MEMBER @@Block:=9
		MEMBER @@Variant:=10
		MEMBER @@VariantOption:=11
		MEMBER @@Method:=12
		MEMBER @@StaticMethod:=13
		MEMBER @@Unknown6:=14
		MEMBER @@Namespace:=15
		MEMBER @@Operator:=16
		MEMBER @@Property:=17
		MEMBER @@Structure:=18
		MEMBER @@Unknown9:=19
		MEMBER @@Macro:=20
		MEMBER @@Unknown11:=21
		MEMBER @@Unknown12:=22
		MEMBER @@Local:=23
		MEMBER @@ClassMethod:=24
	END ENUM
	
	INTERNAL ENUM ImageListOverlay AS Int32
		MEMBER @@Internal:=1
		MEMBER @@ProtectedInternal:=2
		MEMBER @@Protected:=3
		MEMBER @@Private:=4
		MEMBER @@ImageListOverlayArrow:=5
		MEMBER @@Public:=0
	END ENUM
	
	ENUM Kind AS Int32 
		MEMBER @@Namespace
		MEMBER @@Class
		MEMBER @@Structure
		MEMBER @@Constructor
		MEMBER @@Destructor
		MEMBER @@Method
		MEMBER @@Access
		MEMBER @@Assign
		MEMBER @@Property
		MEMBER @@Function
		MEMBER @@Procedure
		MEMBER @@Field
		MEMBER @@Local
		MEMBER @@Parameter
		MEMBER @@Event
		MEMBER @@Operator
		MEMBER @@Interface
		MEMBER @@Delegate
		MEMBER @@Enum
		MEMBER @@EnumMember
		MEMBER @@Keyword
		MEMBER @@Union
		MEMBER @@Using
		MEMBER @@VODefine
		MEMBER @@VODLL
		MEMBER @@VOStruct
		MEMBER @@VOGlobal
		MEMBER @@Unknown
	END ENUM

	[Flags];
	ENUM Modifiers AS LONG
		MEMBER @@None:=0
		MEMBER @@Private:= 0x01
		MEMBER @@Hidden:=  0x01
		MEMBER @@ProtectedInternal:=0x02
		MEMBER @@Internal:=0x04
		MEMBER @@Protected:=0x08
		MEMBER @@Public:=0x10
		MEMBER @@Export:=0x10
		MEMBER @@VisibilityMask := 0xFF
		MEMBER @@Abstract:=0x100
		MEMBER @@New:=0x200
		MEMBER @@Partial:=0x400
		MEMBER @@Sealed:=0x800
		MEMBER @@Static:=0x1000
		MEMBER @@Unsafe:=0x2000
		MEMBER @@Virtual:=0x4000
	END ENUM
	ENUM XFileType AS LONG
		MEMBER Unknown:=-1
		MEMBER SourceCode:=0
		MEMBER PreprocessorOutput:=1
		MEMBER Header:=2
		MEMBER VOForm:=3
		MEMBER VOMenu:=4
		MEMBER VODBServer:=5
		MEMBER VOIndex:=6
		MEMBER VOOrder:=7
		MEMBER VOFieldSpec:=8
		MEMBER NativeResource:=9
		MEMBER ManagedResource:=10
		MEMBER XAML:=11
		MEMBER Settings:=12
		MEMBER License:=13
	END ENUM
	
	ENUM ParamType AS BYTE
		MEMBER @@AS		:= 0
		MEMBER @@Ref    := 1
		MEMBER @@Out	:= 2
		MEMBER @@Params := 3
	END ENUM
END NAMESPACE 

