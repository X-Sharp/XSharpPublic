//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Reflection
BEGIN NAMESPACE XSharp
	/// <summary>
	/// This interface defines Compile time and runtime codeblocks
	/// </summary>
	/// <seealso cref="T:XSharp.Codeblock"/>
	/// <seealso cref="T:XSharp._Codeblock"/>
	INTERFACE ICodeblock
		/// <summary>Evaluate the codeblock</summary>
		METHOD	EvalBlock( args PARAMS OBJECT[]) AS OBJECT
		/// <summary>
		/// Returns the number of parameters defined for the codeblock
		/// </summary>
		METHOD	PCount AS LONG 
    END INTERFACE

    /// <summary>
	/// This delegate is used to decide between 2 ambigous methods or constructors
	/// </summary>
    /// <param name="cSignature1">Signature of the first symbol</param>
    /// <param name="cSignature2">Signature of the second symbol</param>
    /// <returns>The delegate should return 1 when it wants to use the first symbol and 2 when it wants to use the second symbol or 0 when it does not want to use either symbol</returns>
    /// <seealso cref="T:XSharp.IMacroCompiler2"/>
    /// <seealso cref="T:XSharp.IMacroCompiler2"/>
    /// <seealso cref="M:XSharp.Core.Functions.SetMacroDuplicatesResolver(XSharp.MacroCompilerResolveAmbiguousMatch)"/>
    DELEGATE MacroCompilerResolveAmbiguousMatch(m1 as MemberInfo, m2 as MemberInfo, args as System.Type[]) AS LONG

    /// <summary>
	/// This interface extends the Macro compiler and adds a method that is called to decide between ambigous methods or constructors
	/// </summary>
	/// <seealso cref="M:XSharp.Core.Functions.SetMacroDuplicatesResolver(XSharp.MacroCompilerResolveAmbiguousMatch)"/>
	/// <seealso cref="T:XSharp.MacroCompilerResolveAmbiguousMatch"/>
	INTERFACE IMacroCompiler2 INHERIT IMacroCompiler
        PROPERTY Resolver as MacroCompilerResolveAmbiguousMatch GET SET
    END INTERFACE

	/// <summary>
	/// This interface defines the Macro compiler subsystem
	/// </summary>
	/// <seealso cref="M:XSharp.Core.Functions.SetMacroCompiler(System.Type)"/>
	/// <seealso cref="M:XSharp.Core.Functions.GetMacroCompiler"/>
	/// <seealso cref="T:XSharp.ICodeblock"/>
	INTERFACE IMacroCompiler
		/// <summary>Compile a string into a runtime codeblock.</summary>
		/// <param name="macro">String to compile</param>
		/// <param name="lAllowSingleQuotes">Should single quotes be allowed</param>
		/// <param name="module">Module of the main app</param>
		/// <param name="isCodeblock">will be set to TRUE when the string was a real codeblock (with {|..| }).</param>
		/// <param name="addsMemVars">will be set to TRUE when the macro contains code that may result in adding new MemVars).</param>
		/// <returns>A compiled codeblock</returns>
		/// <seealso cref="T:XSharp.ICodeblock"/>
		METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module AS System.Reflection.Module, ;
            isCodeblock OUT LOGIC, addsMemVars OUT LOGIC) AS ICodeblock
    END INTERFACE

    /// <summary>Delegate used for Runtime Codeblocks </summary>
    DELEGATE RuntimeCodeblockDelegate(args PARAMS DYNAMIC[]) AS DYNAMIC

    /// <summary>Class wrapper used for Runtime Codeblocks </summary>
    PUBLIC CLASS RuntimeCodeblock IMPLEMENTS ICodeblock
        PRIVATE _eval AS RuntimeCodeblockDelegate
        PRIVATE _pcount AS INT

        PUBLIC METHOD EvalBlock(args PARAMS DYNAMIC[]) AS DYNAMIC
            RETURN _eval(args)

        PUBLIC METHOD PCount() AS INT
            RETURN _pcount

        PUBLIC CONSTRUCTOR(evalMethod AS RuntimeCodeblockDelegate, pCount AS INT)
            _eval := evalMethod
            _pcount := pCount
    END CLASS
END NAMESPACE
