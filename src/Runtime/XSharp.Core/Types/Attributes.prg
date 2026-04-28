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
/// <include file="XSharp.Core.Docs.xml" path="doc/ActualTypeAttribute.Value/*" />
PROPERTY @@Value AS System.Type GET _value
/// <include file="XSharp.Core.Docs.xml" path="doc/ActualTypeAttribute.ctor/*" />
CONSTRUCTOR(@@Value AS System.Type)
    _value := @@Value

END CLASS

/// <summary>
/// This class exposes the namespace and globalclass in an assembly
/// </summary>
[AttributeUsage(AttributeTargets.Assembly)];
SEALED CLASS ClassLibraryAttribute INHERIT Attribute
PRIVATE INITONLY _globalClassName AS STRING
PRIVATE INITONLY _defaultNameSpace AS STRING
/// <include file="XSharp.Core.Docs.xml" path="doc/ClassLibraryAttribute.GlobalClassName/*" />
PROPERTY GlobalClassName AS STRING GET _globalClassName
/// <include file="XSharp.Core.Docs.xml" path="doc/ClassLibraryAttribute.DefaultNameSpace/*" />
PROPERTY DefaultNameSpace AS STRING GET _defaultNameSpace
/// <include file="XSharp.Core.Docs.xml" path="doc/ClassLibraryAttribute.ctor/*" />
CONSTRUCTOR(globalClassName AS STRING, defaultNameSpace AS STRING)
    _globalClassName := globalClassName
    _defaultNameSpace := defaultNameSpace

END CLASS



/// <summary>
/// This class documents the parameter names for methods and functions
/// with Clipper calling convention
/// </summary>
[AttributeUsage(AttributeTargets.Method|AttributeTargets.Constructor|AttributeTargets.Delegate)];
SEALED CLASS ClipperCallingConventionAttribute INHERIT Attribute
PRIVATE INITONLY _parameterNames AS STRING[]
/// <include file="XSharp.Core.Docs.xml" path="doc/ClipperCallingConventionAttribute.ParameterNames/*" />
PROPERTY ParameterNames AS STRING[] GET _parameterNames
/// <include file="XSharp.Core.Docs.xml" path="doc/ClipperCallingConventionAttribute.ctor/*" />
CONSTRUCTOR(parameterNames AS STRING[])
    _parameterNames := parameterNames

END CLASS


/// <summary>
/// This class records the compiler version used to build an assembly
/// </summary>
[AttributeUsage(AttributeTargets.Assembly)];
SEALED CLASS CompilerVersionAttribute INHERIT Attribute
PRIVATE INITONLY _version AS STRING
/// <include file="XSharp.Core.Docs.xml" path="doc/CompilerVersionAttribute.Version/*" />
PROPERTY Version AS STRING GET _version

/// <include file="XSharp.Core.Docs.xml" path="doc/CompilerVersionAttribute.ctor/*" />
CONSTRUCTOR(version AS STRING)
    _version := version

END CLASS


/// <include file="XSharp.Core.Docs.xml" path="doc/DefaultParameterValueAttribute/*" />
SEALED CLASS DefaultParameterValueAttribute INHERIT Attribute
    PRIVATE INITONLY _value AS OBJECT
    PRIVATE INITONLY _flag  AS INT
/// <include file="XSharp.Core.Docs.xml" path="doc/DefaultParameterValueAttribute.Value/*" />
PROPERTY @@Value AS OBJECT GET _value
/// <include file="XSharp.Core.Docs.xml" path="doc/DefaultParameterValueAttribute.Flag/*" />
PROPERTY Flag  AS INT    GET _flag

/// <include file="XSharp.Core.Docs.xml" path="doc/DefaultParameterValueAttribute.ctor/*" />
CONSTRUCTOR(oValue AS OBJECT, flag AS INT)
    _value := oValue
    _flag  := flag

END CLASS

/// <include file="XSharp.Core.Docs.xml" path="doc/FoxArrayInputParameterAttribute/*" />
SEALED CLASS FoxArrayInputParameterAttribute INHERIT Attribute
    PRIVATE INITONLY _nParameter as INT
    PROPERTY Parameter AS INT GET _nParameter
CONSTRUCTOR(nParameter AS LONG)
    _nParameter := nParameter
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
/// <include file="XSharp.Core.Docs.xml" path="doc/VoStructAttribute.Size/*" />
PROPERTY Size				AS INT GET _size
/// <include file="XSharp.Core.Docs.xml" path="doc/VoStructAttribute.LargestMemberSize/*" />
PROPERTY LargestMemberSize	AS INT GET _largestMemberSize

/// <include file="XSharp.Core.Docs.xml" path="doc/VoStructAttribute.ctor/*" />
CONSTRUCTOR(size AS INT, largestMemberSize AS INT)
    _size := size
    _largestMemberSize := largestMemberSize

END CLASS

/// <summary>
/// This class is used to describe fields that have been declared with the INSTANCE modifier
/// </summary>
[AttributeUsage(AttributeTargets.Field)];
SEALED CLASS IsInstanceAttribute INHERIT Attribute

/// <include file="XSharp.Core.Docs.xml" path="doc/IsInstanceAttribute.ctor/*" />
CONSTRUCTOR()
    SUPER()
    RETURN

END CLASS

/// <summary>
/// This class is used to mark type that allow LateBound calls without the need of the /lb compiler option
/// </summary>
[AttributeUsage(AttributeTargets.Class|AttributeTargets.Struct)];
SEALED CLASS AllowLateBindingAttribute INHERIT Attribute
CONSTRUCTOR()
    SUPER()

END CLASS

/// <summary>
/// This class is used to mark methods, properties etc. that want access to Local Variables by name.
/// </summary>
/// <remarks>
/// In FoxPro several built-in functions have access to locals "by name". We can't do that because it violates
/// the normal rules of encapsulation. However we can "emulate" this behavior by adding this attribute to a function or
/// method that wants access to locals.
/// At runtime we will then populate a table in the runtime with the names and values of local variables
/// And we can access this table with the normal MemVarGet() and MemVarPut() functions.
/// </remarks>
/// <seealso cref="O:XSharp.RT.Functions.Type" />
/// <seealso cref="O:XSharp.RT.Functions.MExec" />
/// <seealso cref="O:XSharp.RT.Functions.Evaluate" />
/// <seealso cref="O:XSharp.RT.Functions.StrEvaluate" />
/// <seealso cref="O:XSharp.VFP.Functions.SqlExec" />
/// <seealso cref="O:XSharp.VFP.Functions.SqlPrepare" />
[AttributeUsage(AttributeTargets.Method | AttributeTargets.Property | AttributeTargets.Constructor) ];
SEALED CLASS NeedsAccessToLocalsAttribute INHERIT Attribute
PRIVATE _writesToLocals := FALSE AS LOGIC
PROPERTY WritesToLocals AS LOGIC GET _writesToLocals
/// <include file="XSharp.Core.Docs.xml" path="doc/NeedsAccessToLocalsAttribute.ctor/*" />
CONSTRUCTOR(lWrites AS LOGIC)
    _writesToLocals := lWrites

END CLASS


/// <summary>
/// This class is used to mark a class and indicate that the property types and or parameter types in this class were changed.
/// When the compiler detects that a subclass does not have the right parameter or property types then the subclass will be automatically
/// adjusted to preserved the types from the parent class.
/// </summary>
[AttributeUsage(AttributeTargets.Class) ];
SEALED CLASS TypesChangedAttribute INHERIT Attribute
CONSTRUCTOR()
    SUPER()
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
    [AttributeUsage(AttributeTargets.Assembly, AllowMultiple := true)];
    SEALED CLASS ImplicitNamespaceAttribute INHERIT Attribute
    PRIVATE INITONLY _namespace AS STRING
/// <include file="XSharp.Core.Docs.xml" path="doc/ImplicitNamespaceAttribute.Namespace/*" />
PROPERTY Namespace AS STRING GET _namespace

/// <include file="XSharp.Core.Docs.xml" path="doc/ImplicitNamespaceAttribute.ctor/*" />
CONSTRUCTOR(namespace AS STRING)
    _namespace := namespace

END CLASS

END NAMESPACE
