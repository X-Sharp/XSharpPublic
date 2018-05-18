//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp
	/// <summary>
	/// This interface defines Compile time and runtime codeblocks
	/// </summary>
	INTERFACE ICodeBlock
		METHOD	EvalBlock( args PARAMS OBJECT[]) AS OBJECT
		METHOD	PCount AS LONG 
	END INTERFACE

	/// <summary>
	/// This interface defines Date values
	/// </summary>
	INTERFACE IDate
		/// <summary>Year part of the date. A number between 0 and 9999</summary>
		/// <returns>Integer value</returns>
		PROPERTY Year		AS INT GET
		/// <summary>Month part of the date. A number between 0 an 12</summary>
		/// <returns>Integer value</returns>
		PROPERTY Month		AS INT GET
		/// <summary>Day part of the date. A number between 0 an 31</summary>
		/// <returns>Integer value</returns>
		PROPERTY Day		AS INT GET
		/// <summary>Date as System.DateTime structure</summary>
		/// <returns>System.DateTime value</returns>
		PROPERTY Value		AS DateTime GET 
		/// <summary>Is the date empty (NULL_DATE)</summary>
		/// <returns>Logical value</returns>
		PROPERTY IsEmpty	AS LOGIC GET
	END INTERFACE

	/// <summary>
	/// This interface defines FLOAT values
	/// </summary>
	INTERFACE IFloat
		/// <summary>Double value of the Float</summary>
		/// <returns>Integer value</returns>
		PROPERTY Value    AS REAL8 GET
		/// <summary>Number of digits (includes the optional decimal separator and decimals).</summary>
		/// <returns>Integer value</returns>
		PROPERTY Digits	  AS INT  GET 
		/// <summary>Number of decimals.</summary>
		/// <returns>Integer value</returns>
		PROPERTY Decimals AS INT  GET 
	END INTERFACE
	/// <summary>
	/// This interface defines the Macro compiler subsystem
	/// </summary>
	INTERFACE IMacroCompiler
		/// <summary>Compile a string into a runtime codeblock.</summary>
		/// <param Name="macro">String to compile</param>
		/// <param Name="lAllowSingleQuotes">Should single quotes be allowed</param>
		/// <param Name="module">Module of the main app</param>
		/// <param Name="isCodeblock">will be set to TRUE when the string was a real codeblock (with {|..| }).</param>
		/// <returns>A compiled codeblock</returns>
		/// <seealso cref="T:XSharp.ICodeblock"/>
		METHOD Compile(macro AS STRING , lAllowSingleQuotes AS LOGIC, module as System.Reflection.Module, isCodeblock OUT LOGIC) AS ICodeBlock
	END INTERFACE
END NAMESPACE