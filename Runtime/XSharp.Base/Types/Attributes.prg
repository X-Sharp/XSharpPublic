//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp.Internal



/// <summary>
/// This class is used to mark IntPtr _DLL arguments with the actual type
/// </summary>
[AttributeUsage(AttributeTargets.Parameter)];
SEALED CLASS ActualTypeAttribute INHERIT Attribute
	PRIVATE _value as System.Type

	PROPERTY Value as System.Type get _value

	CONSTRUCTOR(value as System.Type)
		_value := value

END CLASS

/// <summary>
/// This class exposes the namespace and globalclass in an assembly
/// </summary>
[AttributeUsage(AttributeTargets.Assembly)];
SEALED CLASS ClassLibraryAttribute INHERIT Attribute
	PRIVATE _globalClassName as STRING
	PRIVATE _defaultNameSpace as STRING

	PROPERTY GlobalClassName AS STRING GET _globalClassName
	PROPERTY DefaultNameSpace AS STRING GET _defaultNameSpace

	CONSTRUCTOR(globalClassName AS STRING, defaultNameSpace AS STRING)
		_globalClassName := globalClassName
		_defaultNameSpace := defaultNameSpace

END CLASS


/// <summary>
/// this class documents the parameter names for methods and functions
/// with Clipper calling convention
/// </summary>

[AttributeUsage(AttributeTargets.Method)];
SEALED CLASS ClipperCallingConventionAttribute INHERIT Attribute
	PRIVATE _parameterNames as STRING[]

	PROPERTY ParameterNames as STRING[] GET _parameterNames

	CONSTRUCTOR(parameterNames as STRING[])
		_parameterNames := parameterNames

END CLASS


/// <summary>
/// this class records the compiler version used to build an assembly
/// </summary>
[AttributeUsage(AttributeTargets.Assembly)];
SEALED CLASS CompilerVersionAttribute INHERIT Attribute
	PRIVATE _version as STRING

	PROPERTY Version as STRING GET _version

	CONSTRUCTOR(version as STRING)
		_version := version

END CLASS


/// <summary>
/// this class is used to mark default parameter values in the middle of a parameter list
/// Value is obvious
/// Flag means:
/// 0 = Regular .Net default value 
/// 1 = NIL
/// 2 = Date (value is LONG ticks, empty for NULL_DATE
/// 3 = Symbol, value can be empty of a string
/// 4 = PSZ, null = empty, or a string
/// 5 = IntPtr (can be 0 for NullPtr)
/// </summary>
SEALED CLASS DefaultParameterValueAttribute INHERIT Attribute
	PRIVATE _value as Object
	PRIVATE _flag  as INT

	PROPERTY Value as OBJECT get _value
	PROPERTY Flag  as INT    get _flag

	CONSTRUCTOR(value as Object, flag as INT)
		_value := value
		_flag  := flag

END CLASS


/// <summary>
/// this class records the compiler version used to build an assembly
/// </summary>
[AttributeUsage(AttributeTargets.Assembly)];
SEALED CLASS ImplicitNamespaceAttribute INHERIT Attribute
	PRIVATE _namespace AS STRING

	PROPERTY Namespace as STRING get _namespace

	CONSTRUCTOR(namespace as STRING)
		_namespace := namespace

END CLASS


/// <summary>
/// this class is used to describe VOSTRUCT and UNION structures which have a different semantics
/// AS actually means Struct PTR. IS means Struct
/// Please note that the size assumes x86 (32 bits) OS.
/// </summary>
[AttributeUsage(AttributeTargets.Struct)];
SEALED CLASS VoStructAttribute INHERIT Attribute
	PRIVATE _size				as INT
	PRIVATE _largestMemberSize  as INT
	
	PROPERTY Size				as INT GET _size
	PROPERTY LargestMemberSize	as INT GET _largestMemberSize

	CONSTRUCTOR(size as Int, largestMemberSize as INT)
		_size := size
		_largestMemberSize := largestMemberSize

END CLASS

END NAMESPACE