//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp
	/// <summary>
	/// This interface defines Compile time and runtime codeblocks
	/// </summary>
	/// <seealso cref="T:XSharp.CodeBlock"/>
	/// <seealso cref="T:XSharp._CodeBlock"/>
	INTERFACE ICodeBlock
		/// <summary>Evaluate the codeblock</summary>
		METHOD	EvalBlock( args PARAMS OBJECT[]) AS OBJECT
		/// <summary>
		/// Returns the number of parameters defined for the codeblock
		/// </summary>
		METHOD	PCount AS LONG 
	END INTERFACE
	/// <summary>
	/// This interface defines the Macro compiler subsystem
	/// </summary>
	/// <seealso cref="M:XSharp.Core.Functions.SetMacroCompiler(System.Type)"/>
	/// <seealso cref="M:XSharp.Core.Functions.GetMacroCompiler"/>
	/// <seealso cref="T:XSharp.ICodeBlock"/>
	INTERFACE IMacroCompiler
		/// <summary>Compile a string into a runtime codeblock.</summary>
		/// <param name="macro">String to compile</param>
		/// <param name="lAllowSingleQuotes">Should single quotes be allowed</param>
		/// <param name="module">Module of the main app</param>
		/// <param name="isCodeblock">will be set to TRUE when the string was a real codeblock (with {|..| }).</param>
		/// <returns>A compiled codeblock</returns>
		/// <seealso cref="T:XSharp.ICodeBlock"/>
		METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module AS System.Reflection.Module, isCodeblock OUT LOGIC) AS ICodeBlock
    END INTERFACE

    /// <summary>Delegate used for Runtime Codeblocks </summary>
    DELEGATE RuntimeCodeblockDelegate(args PARAMS DYNAMIC[]) AS DYNAMIC

    /// <summary>Class wrapper used for Runtime Codeblocks </summary>
    PUBLIC CLASS RuntimeCodeblock IMPLEMENTS ICodeBlock
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
