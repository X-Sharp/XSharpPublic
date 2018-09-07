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
		PRIVATE INITONLY _value AS System.Type
		///<summary>The actual type of the parameter.</summary>
		PROPERTY VALUE AS System.Type GET _value
		
		CONSTRUCTOR(VALUE AS System.Type)
			_value := VALUE
		
	END CLASS
	
	/// <summary>
	/// This class exposes the namespace and globalclass in an assembly
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly)];
		SEALED CLASS ClassLibraryAttribute INHERIT Attribute
		PRIVATE INITONLY _globalClassName AS STRING
		PRIVATE INITONLY _defaultNameSpace AS STRING
		///<summary>Name of the class where functions, defines and globals are stored.</summary>
		PROPERTY GlobalClassName AS STRING GET _globalClassName
        ///<summary>Default namespace of the assembly.</summary>
		PROPERTY DefaultNameSpace AS STRING GET _defaultNameSpace
		
		CONSTRUCTOR(globalClassName AS STRING, defaultNameSpace AS STRING)
			_globalClassName := globalClassName
			_defaultNameSpace := defaultNameSpace
		
	END CLASS
	
	
	/// <summary>
	/// This class documents the parameter names for methods and functions
	/// with Clipper calling convention
	/// </summary>
	
	[AttributeUsage(AttributeTargets.Method)];
		SEALED CLASS ClipperCallingConventionAttribute INHERIT Attribute
		PRIVATE INITONLY _parameterNames AS STRING[]
		///<summary>List of parameter names for the method or constructor </summary>
		PROPERTY ParameterNames AS STRING[] GET _parameterNames
		
		CONSTRUCTOR(parameterNames AS STRING[])
			_parameterNames := parameterNames
		
	END CLASS
	
	
	/// <summary>
	/// This class records the compiler version used to build an assembly
	/// </summary>
	[AttributeUsage(AttributeTargets.Assembly)];
	SEALED CLASS CompilerVersionAttribute INHERIT Attribute
		PRIVATE INITONLY _version AS STRING
	    ///<summary>The version and dialect of the compiler that created the assembly.</summary>	
		PROPERTY Version AS STRING GET _version
		
		CONSTRUCTOR(version AS STRING)
			_version := version
		
	END CLASS
	
	
	/// <summary>
	/// this class is used to mark default parameter values in the middle of a parameter list.
    /// </summary>
    /// <remarks>
    /// It is used when compiling in VO or Vulcan mode.
    /// And only for parameters that are followed by parameters without default value (because that is not supported by the .Net framework).<br/>
    /// The X# compiler will find this attribute at compile time and will insert its value into the argument list.
    /// </remarks>
	SEALED CLASS DefaultParameterValueAttribute INHERIT Attribute
		PRIVATE INITONLY _value AS OBJECT
		PRIVATE INITONLY _flag  AS INT
		/// <summary>Value of the default parameter</summary>
		PROPERTY VALUE AS OBJECT GET _value
        /// <summary>Flag indicating the type of the default parameter</summary>
        /// <remarks>
	    /// 0 = Regular .Net default value 
	    /// 1 = NIL
	    /// 2 = Date (value is LONG ticks, empty for NULL_DATE)
	    /// 3 = Symbol, value can be empty or a string
	    /// 4 = PSZ, null = empty, or a string
	    /// 5 = IntPtr (can be 0 for NullPtr)
	    /// </remarks>
		PROPERTY Flag  AS INT    GET _flag
		
		CONSTRUCTOR(VALUE AS OBJECT, flag AS INT)
			_value := VALUE
			_flag  := flag
		
	END CLASS
	
	/// <summary>
	/// This class is used to describe VOSTRUCT and UNION structures which have a different semantics
	/// AS actually means Struct PTR. IS means Struct
	/// Please note that the size assumes x86 (32 bits) OS.
	/// </summary>
	[AttributeUsage(AttributeTargets.Struct)];
		SEALED CLASS VoStructAttribute INHERIT Attribute
		PRIVATE _size				AS INT
		PRIVATE _largestMemberSize  AS INT
		///<summary>Total size of the VOSTRUCT or UNION.</summary>	
		PROPERTY Size				AS INT GET _size
        ///<summary>Size of the largest member.</summary>	
		PROPERTY LargestMemberSize	AS INT GET _largestMemberSize
		
		CONSTRUCTOR(size AS INT, largestMemberSize AS INT)
			_size := size
			_largestMemberSize := largestMemberSize
		
	END CLASS
	
END NAMESPACE


BEGIN NAMESPACE XSharp
	/// <summary>
	/// This class is used to tell the compiler that the assembly has types in a specific namespace.
    /// When the compiler is called with the /ins command line option, then the names specified with this attribute
    /// will be automatically included in the USING list when compiling.
    /// </summary>
    /// <remarks>
	/// This type is not in the internal namespace because it is supposed to be used by 3rd party vendors as well
	/// </remarks>
	[AttributeUsage(AttributeTargets.Assembly)];
	SEALED CLASS ImplicitNamespaceAttribute INHERIT Attribute
		PRIVATE INITONLY _namespace AS STRING
	    ///<summary>Name of the implicit namespace.</summary>		
		PROPERTY Namespace AS STRING GET _namespace
		
		CONSTRUCTOR(namespace AS STRING)
			_namespace := namespace
		
	END CLASS
	
END NAMESPACE
