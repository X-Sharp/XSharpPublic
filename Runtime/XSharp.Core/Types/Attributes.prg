//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Internal
	
	
	
	/// <summary>
	/// This class is used to mark IntPtr _DLL arguments with the actual type
	/// </summary>
	[AttributeUsage(AttributeTargets.Parameter)];
		sealed class ActualTypeAttribute inherit Attribute
		private initonly _value as System.Type
		
		property Value as System.Type get _value
		
		constructor(value as System.Type)
			_value := value
		
	end class
	
	/// <summary>
	/// This class exposes the namespace and globalclass in an assembly
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly)];
		sealed class ClassLibraryAttribute inherit Attribute
		private initonly _globalClassName as string
		private initonly _defaultNameSpace as string
		
		property GlobalClassName as string get _globalClassName
		property DefaultNameSpace as string get _defaultNameSpace
		
		constructor(globalClassName as string, defaultNameSpace as string)
			_globalClassName := globalClassName
			_defaultNameSpace := defaultNameSpace
		
	end class
	
	
	/// <summary>
	/// this class documents the parameter names for methods and functions
	/// with Clipper calling convention
	/// </summary>
	
	[AttributeUsage(AttributeTargets.Method)];
		sealed class ClipperCallingConventionAttribute inherit Attribute
		private initonly _parameterNames as string[]
		
		property ParameterNames as string[] get _parameterNames
		
		constructor(parameterNames as string[])
			_parameterNames := parameterNames
		
	end class
	
	
	/// <summary>
	/// this class records the compiler version used to build an assembly
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly)];
		sealed class CompilerVersionAttribute inherit Attribute
		private initonly _version as string
		
		property Version as string get _version
		
		constructor(version as string)
			_version := version
		
	end class
	
	
	/// <summary>
	/// this class is used to mark default parameter values in the middle of a parameter list
	/// Value is obvious
	/// Flag means:
	/// 0 = Regular .Net default value 
	/// 1 = NIL
	/// 2 = Date (value is LONG ticks, empty for NULL_DATE)
	/// 3 = Symbol, value can be empty or a string
	/// 4 = PSZ, null = empty, or a string
	/// 5 = IntPtr (can be 0 for NullPtr)
	/// </summary>
	sealed class DefaultParameterValueAttribute inherit Attribute
		private initonly _value as object
		private initonly _flag  as int
		
		property Value as object get _value
		property Flag  as int    get _flag
		
		constructor(value as object, flag as int)
			_value := value
			_flag  := flag
		
	end class
	
	
	/// <summary>
	/// this class records the compiler version used to build an assembly
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly)];
		sealed class ImplicitNamespaceAttribute inherit Attribute
		private initonly _namespace as string
		
		property Namespace as string get _namespace
		
		constructor(namespace as string)
			_namespace := namespace
		
	end class
	
	
	/// <summary>
	/// this class is used to describe VOSTRUCT and UNION structures which have a different semantics
	/// AS actually means Struct PTR. IS means Struct
	/// Please note that the size assumes x86 (32 bits) OS.
	/// </summary>
	[AttributeUsage(AttributeTargets.Struct)];
		sealed class VoStructAttribute inherit Attribute
		private _size				as int
		private _largestMemberSize  as int
		
		property Size				as int get _size
		property LargestMemberSize	as int get _largestMemberSize
		
		constructor(size as int, largestMemberSize as int)
			_size := size
			_largestMemberSize := largestMemberSize
		
	end class
	
end namespace
