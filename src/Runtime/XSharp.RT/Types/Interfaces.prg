//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

begin namespace XSharp
/// <summary>This interface can be used to access any object with an indexer.
/// This is mostly used for elements inside typed arrays of the 'ARRAY OF' class.
/// If you implement the interface on the elements you can use an array syntax to
/// assess fields/properties in the elements of the array by name or ordinal.</summary>
/// <seealso cref='INamedIndexer' />
/// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
interface IIndexedProperties
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    property self[index as int   ] as usual get set
    /// <summary>Get/Set array elements with a name.</summary>
    /// <param name="name">Name which will be used to lookup a property</param>
    property self[name  as string] as usual get set
end interface

/// <summary> This interface is used to index a collection using the VO Array syntax.
/// The interface is implemented by the ARRAY type in the runtime, but you can also use it for your custom types.</summary>
/// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
/// <seealso cref='T:XSharp.__Array' />
interface IIndexer
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    public property self[index params int[]] as usual get set
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    public property self[index as int] as usual get set
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index1"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="index2"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    public property self[index1 as int, index2 as int] as usual get set
end interface

/// <summary>This interface is used to index a collection using a numeric and a string index and is implemented by the
/// typed array class ('ARRAY OF'). If your elements inside the collection implement the IIndexProperties interface then
/// the lookup of the property inside array element will be resolved with a call to the named indexer on that object.</summary>
/// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
/// <seealso cref='T:XSharp.IIndexedProperties' />
/// <seealso cref='T:XSharp.__ArrayBase`1' />
interface INamedIndexer
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="name"><include file="RTComments.xml" path="Comments/NameBasedIndexParam/*" /></param>
    public property self[index as int, name as string] as usual get set
end interface

/// <summary> This interface is used for objects that implement a special mechanism
/// for reading / writing properties at runtime.</summary>

interface IDynamicProperties
    /// <summary>Retrieve an array of the property names that exist.</summary>
    public method GetPropertyNames() as string[]
    /// <summary>Retrieve the value of a property by name.</summary>
    /// <param name="cName">Property Name</param>
    /// <returns>The current value of the property </returns>
    /// <remarks>Accessing a property that does not exist will result in a runtime error.</remarks>
    public method NoIvarGet(cName as string) as usual
    /// <summary>Update the value of a property by name.</summary>
    /// <param name="cName">Property Name</param>
    /// <param name="uValue">New value of the property</param>
    /// <remarks>Assigning a property that does not exist may result in a runtime error. <br/>
    /// However the class may also decide to "silently" add the property.</remarks>
    public method NoIvarPut(cName as string, uValue as usual) as void
end interface

interface IDynamicProperties2
    public method _RemoveProperty(cName as string) as logic
    public method _AddProperty(cPropertyName as string, uValue as usual, nVisibility as long, cDescription as string) as logic

end interface

/// <summary>
/// This interface defines and extension to the Macro compiler subsystem
/// </summary>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
/// <seealso cref="T:XSharp.IMacroCompiler2"/>
interface IMacroCompilerUsual
    /// <summary>Compile a string into a runtime codeblock.</summary>
    /// <param name="macro">String to compile</param>
    /// <param name="lAllowSingleQuotes">Should single quotes be allowed</param>
    /// <param name="module">Module of the main app</param>
    /// <param name="isCodeblock">will be set to TRUE when the string was a real codeblock (with {|..| }).</param>
    /// <param name="addsMemVars">will be set to TRUE when the macro contains code that may result in adding new MemVars).</param>
    /// <returns>A compiled codeblock</returns>
    /// <seealso cref="T:XSharp._Codeblock"/>
    public method CompileCodeblock(macro as string , lAllowSingleQuotes as logic, module as System.Reflection.Module) as XSharp._Codeblock

    /// <summary>Compile a string into a runtime codeblock.</summary>
    /// <param name="macro">String to compile</param>
    /// <returns>A compiled codeblock</returns>
    /// <seealso cref="T:XSharp._Codeblock"/>
    /// <remarks>This overload of the CompileCodeBlock assumes that single quotes are allowed, and that no memvars are used.</remarks>
    public method CompileCodeblock(macro as string ) as XSharp._Codeblock

end interface

/// <summary>
/// This interface Extended the ICodeblock interface and adds support for usual
/// parameters and return values
/// </summary>
/// <seealso cref="T:XSharp.ICodeblock"/>
interface IRtCodeblock inherit ICodeblock2
    /// <summary>Was the codeblock created from a string that started with "{|" </summary>
    property IsBlock as logic get
    /// <summary>
    /// Executes the codeblock.</summary>
    /// <param name="args">Zero or more arguments to pass to the codeblock.</param>
    /// <returns>The value of the last expression in the codeblock.</returns>
    method Eval(args params usual[]) as usual
end interface

/// <summary>
/// This interface described the methods needed for a LateBound object <br/>
/// When an object implements this interface then the X# runtime will not use
/// reflection to implement late bound calls to properties or methods
/// but will call these methods instead.
/// </summary>
interface ILateBound
    /// <summary>
    /// Method to read a field or property late bound.
    /// </summary>
    /// <param name="cName">Field/Property Name</param>
    /// <returns>Current value of Field/Property</returns>
    method NoIvarGet(cName as string) as usual
    /// <summary>
    /// Method to update a field or property late bound.
    /// </summary>
    /// <param name="cName">Field/Property Name</param>
    /// <param name="uValue">New value for field/property</param>
    method NoIvarPut(cName as string, uValue as usual) as void
    /// <summary>
    /// Call a method in the type late bound. Arguments are passed as Clipper calling convention parameters.
    /// The method name is passed as first parameter.
    /// </summary>
    /// <returns>Result of MethodCall</returns>
    method NoMethod() as usual clipper
END INTERFACE
interface IWrappedObject
    property Object as object get
    property Type   as System.Type get
end interface

end namespace
