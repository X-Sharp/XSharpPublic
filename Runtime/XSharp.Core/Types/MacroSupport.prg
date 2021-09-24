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
    /// <seealso cref="Codeblock"/>
    INTERFACE ICodeblock
        /// <summary>Evaluate the codeblock</summary>
        METHOD	EvalBlock( args PARAMS OBJECT[]) AS OBJECT
        /// <summary>
        /// Returns the number of parameters defined for the codeblock
        /// </summary>
        METHOD	PCount AS LONG
    END INTERFACE

    /// <summary>
	/// This delegate is used to decide between 2 ambigous methods, functions or constructors
	/// </summary>
    /// <param name="m1">MethodInfo of the first symbol</param>
    /// <param name="m2">MethodInfo of the second symbol</param>
    /// <param name="args">The types of the arguments that were passed to the method call</param>
    /// <returns>The delegate should return 1 when it wants to use the first symbol and 2 when it wants to use the second symbol or 0 when it does not want to use either symbol</returns>
    /// <seealso cref="IMacroCompiler2"/>
    /// <seealso cref="IMacroCompiler2"/>
    /// <seealso cref="SetMacroDuplicatesResolver"/>
    DELEGATE MacroCompilerResolveAmbiguousMatch(m1 AS MemberInfo, m2 AS MemberInfo, args AS System.Type[]) AS LONG

    /// <summary>
	/// This interface extends the Macro compiler and adds a method that is called to decide between ambigous methods or constructors
	/// </summary>
	/// <seealso cref="SetMacroDuplicatesResolver"/>
	/// <seealso cref="MacroCompilerResolveAmbiguousMatch"/>
	INTERFACE IMacroCompiler2 INHERIT IMacroCompiler
        /// <summary>This property allows to set a delegate that may be used to resolve ambigous references</summary>
        PROPERTY Resolver AS MacroCompilerResolveAmbiguousMatch GET SET
    END INTERFACE

	/// <summary>
	/// This interface defines the Macro compiler subsystem
	/// </summary>
	/// <seealso cref="SetMacroCompiler"/>
	/// <seealso cref="GetMacroCompiler"/>
	/// <seealso cref="ICodeblock"/>
	INTERFACE IMacroCompiler
		/// <summary>Compile a string into a runtime codeblock.</summary>
		/// <param name="macro">String to compile</param>
		/// <param name="lAllowSingleQuotes">Should single quotes be allowed</param>
		/// <param name="module">Module of the main app</param>
		/// <param name="isCodeblock">will be set to TRUE when the string was a real codeblock (with {|..| }).</param>
		/// <param name="addsMemVars">will be set to TRUE when the macro contains code that may result in adding new MemVars).</param>
		/// <returns>A compiled codeblock</returns>
		/// <seealso cref="ICodeblock"/>
		METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module AS System.Reflection.Module, ;
            isCodeblock OUT LOGIC, addsMemVars OUT LOGIC) AS ICodeblock
    END INTERFACE

END NAMESPACE
