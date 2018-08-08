//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.IO

INTERNAL STATIC CLASS MacroHelpers
	STATIC PRIVATE oMacroCompiler		AS IMacroCompiler
	STATIC PRIVATE oMacroCompilerType	AS System.Type
	STATIC PROPERTY Compiler AS IMacroCompiler
		GET
			LOCAL oMCType AS System.Type
			oMCType  := XSharp.RuntimeState.MacroCompiler
			IF oMCType == NULL_OBJECT 
				MacroHelpers.Initialize()
				oMCType := XSharp.RuntimeState.MacroCompiler
			ENDIF
			IF oMcType != oMacroCompilerType
				oMacroCompilerType := oMcType
				oMacroCompiler     := NULL_OBJECT
			ENDIF
			IF oMacroCompiler == NULL_OBJECT
				oMacroCompiler := Activator:CreateInstance(oMCType) ASTYPE IMacroCompiler
			ENDIF
			RETURN oMacroCompiler
		END GET
	END PROPERTY

	STATIC METHOD Initialize() AS VOID
		LOCAL oMacroAsm		 := NULL_OBJECT AS Assembly
		// first locate the assembly that has the macro compiler in the list of loaded assemblies
		FOREACH oAsm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
			IF oAsm:GetName():Name:ToLower() == "xsharp.macrocompiler"
				oMacroAsm := oAsm
				EXIT
			ENDIF
		NEXT
		IF oMacroAsm == NULL_OBJECT
			LOCAL oVOAsm	AS Assembly
			LOCAL cFileName AS STRING
			oVOAsm := TYPEOF(XSharp.VO.Functions):Assembly
			cFileName := oVOASM:Location:ToLower()
			cFileName := cFileName:Replace("xsharp.vo", "xsharp.macrocompiler")
			IF File.Exists(cFileName)
				TRY
					oMacroAsm := Assembly.LoadFrom(cFileName)
				CATCH  AS Exception
					THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler from the file "+cFileName}
				END TRY
			ENDIF
		ENDIF
		IF oMacroAsm == NULL_OBJECT
			// locate the macro compiler in the GAC or in the path
			LOCAL cPath AS STRING
			cPath := System.Environment.GetEnvironmentVariable("PATH")
			LOCAL aPaths AS STRING[]
			aPaths := cPath:Split(';',StringSplitOptions.RemoveEmptyEntries)
			FOREACH VAR path IN aPaths
				LOCAL cFileName AS STRING
				cFileName := System.IO.Path.Combine(path, "XSharp.MacroCompiler.dll")
				IF System.IO.File.Exists(cFileName)
					TRY
						oMacroAsm := Assembly.LoadFrom(cFileName)
						EXIT
					CATCH  AS Exception
						THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler from the file "+cFileName}
					END TRY
				ENDIF
			NEXT
		ENDIF
		IF oMacroAsm == NULL_OBJECT
			// try to load from the GAC
			LOCAL oAsm AS Assembly
			LOCAL oName AS AssemblyName
			LOCAL cName AS STRING
			oAsm := TYPEOF(XSharp.VO.Functions):Assembly
			oName := oAsm:GetName()
			cName := oName:FullName
			cName := cName:Replace("XSharp.VO", "XSharp.MacroCompiler")
			TRY
				oMacroAsm := Assembly.Load(cName)
			CATCH AS Exception
				THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler with strong name "+cName}
			END TRY
		ENDIF
		IF oMacroAsm != NULL_OBJECT
			LOCAL oType AS System.Type
			oType := oMacroAsm:GetType("XSharp.MacroCompiler",FALSE,TRUE)
			IF oType != NULL_OBJECT
				// create instance of this type
				IF TYPEOF(IMacroCompiler):IsAssignableFrom(oType)
					XSharp.RuntimeState.MacroCompiler := oType
				ELSE
					THROW Error{EG_CORRUPTION, "", "Could not create the macro compiler from the type "+ otype:Fullname+" in the assembly "+oMacroAsm:Location}
				ENDIF
			ELSE
				THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler class in the assembly "+oMacroAsm:Location}
			ENDIF
		ENDIF
		RETURN
		
END	CLASS
		
		
		
/// <summary>
/// Evaluate an expression contained in a string.
/// </summary>
/// <param name="cExpression">The string containing the expression to evaluate.</param>
/// <returns>The value of the expression.</returns>
/// <remarks>Evaluate() invokes the macro compiler each time it evaluates an expression.  Alternatively, you could use MCompile() to compile an expression only once, then use MExec() to execute the compiled form as often as you want.</remarks>
/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String)" />
FUNCTION Evaluate(cExpression AS STRING) AS USUAL
	RETURN Evaluate(cExpression, TRUE)
	
/// <summary>
/// Evaluate an expression contained in a string.
/// </summary>
/// <param name="cExpression">The string containing the expression to evaluate.</param>
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters.</param>
/// <returns>The value of the expression.</returns>
/// <remarks>Evaluate() invokes the macro compiler each time it evaluates an expression.  Alternatively, you could use MCompile() to compile an expression only once, then use MExec() to execute the compiled form as often as you want.</remarks>
/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String,System.Boolean)" />
FUNCTION Evaluate(cExpression AS STRING, lAllowSingleQuotes AS LOGIC) AS USUAL
	LOCAL oMacro AS XSharp._CODEBLOCK 
	LOCAL uRes   AS USUAL
	oMacro := MCompile(cExpression, lAllowSingleQuotes)
	IF oMacro != NULL_OBJECT .AND. ! oMacro:IsBlock
		uRes := oMacro:EvalBlock()
	ELSE
		// strange but evaluate on a codeblock returns the block in stead of evaluating it
		uRes := oMacro
	ENDIF
	RETURN uRes
	
	
/// <summary>
/// Macro compile a string.
/// </summary>
/// <param name="cMacro">The string to compile.</param>
/// <returns>The string in a macro-compiled form.</returns>
/// <remarks>MCompile() allows you to use the macro compiler to compile a string and store the compiled results for later execution.  Instead of invoking the macro compiler each time an expression is evaluated, you could speed up your application by compiling an expression only once and executing the compiled form as often as desired.</remarks>
/// <note type="caution">MCompile returns a STRING in VO. It returns a XSharp._CodeBlock in .Net.</note>
/// <seealso cref="T:XSharp._CodeBlock" />
/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String,System.Boolean)" />
/// <seealso cref="M:XSharp.VO.Functions.MExec(XSharp.CodeBlock)" />
FUNCTION MCompile(cMacro AS STRING) AS XSharp._CODEBLOCK
	RETURN MCompile(cMacro, TRUE)
	
/// <summary>
/// Macro compile a string.
/// </summary>
/// <param name="cMacro">The string to compile.</param>
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters</param>
/// <returns>The string in a macro-compiled form.</returns>
/// <remarks>MCompile() allows you to use the macro compiler to compile a string and store the compiled results for later execution.  Instead of invoking the macro compiler each time an expression is evaluated, you could speed up your application by compiling an expression only once and executing the compiled form as often as desired.</remarks>
/// <note type="caution">MCompile returns a STRING in VO. It returns a XSharp._CodeBlock in .Net.</note>
/// <seealso cref="T:XSharp._CodeBlock" />
/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String)" />
/// <seealso cref="M:XSharp.VO.Functions.MExec(XSharp.CodeBlock)" />
FUNCTION MCompile(cMacro AS STRING, lAllowSingleQuotes AS LOGIC) AS XSharp._CodeBlock
	
	VAR oMC := MacroHelpers.Compiler
	IF oMC != NULL_OBJECT
		VAR oMod := XSharp.RuntimeState.AppModule
		IF oMod == NULL_OBJECT
			XSharp.RuntimeState.AppModule := TYPEOF(XSharp.Core.Functions):Module
		ENDIF
		LOCAL iResult AS ICodeBlock
		LOCAL oResult AS XSharp._CodeBlock
		LOCAL lIsCodeblock AS LOGIC
		iResult := oMC:Compile(cMacro, lAllowSingleQuotes, oMod, OUT lIsCodeBlock)
		oResult := XSharp._CodeBlock{iResult, cMacro, lIsCodeBlock}
		RETURN oResult
	ENDIF
	RETURN NULL_OBJECT	
	
	
	/// <summary>
	/// Evaluate a macro-compiled codeblock.
	/// </summary>
	/// <param name="cb">The macro-compiled codeblock.</param>
	/// <returns>The result of evalating the macro compiled expression.</returns>
	/// <note type="caution">MCompile returns a STRING in VO. It returns a XSharp._CodeBlock in .Net. Therefore the parameter of MExec is a Codeblock</note>
	/// <seealso cref="T:XSharp._CodeBlock" />
	/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String)" />
	/// <seealso cref="M:XSharp.VO.Functions.MCompile(System.String,System.Boolean)" />
FUNCTION MExec(cb AS CODEBLOCK) AS USUAL
	IF cb:PCount() == -1
		RETURN cb:EvalBlock()
	ENDIF
	RETURN cb
	
	
	
	
	/// <summary>
	/// Determine the data type of an expression represented as a string.
	/// </summary>
	/// <param name="cExpression">A string that contains an expression whose type is to be determined.  It cannot contain undeclared variables or functions that are not intended for used with macros  If cExpression does not exist, "U" is returned. </param>
	/// <returns>One of the following characters:
	/// <list type="table">
	/// <listheader>
	/// <term>Returns</term> <description>Meaning</description>
	/// </listheader>
	///	<item><term>A</term> <description>Array</description></item>  
	/// <item><term>B</term> <description>Block</description></item>  
	/// <item><term>C</term> <description>String</description></item>   
	/// <item><term>D</term> <description>Date</description></item> 
	/// <item><term>L</term> <description>Logical</description></item>   
	/// <item><term>M</term> <description>Memo </description></item>
	/// <item><term>N</term> <description>Numeric</description></item>  
	/// <item><term>O</term> <description>Object</description></item> 
	/// <item><term>U</term> <description>NIL, local, or static</description></item>   
	/// <item><term>UE</term> <description>Error syntactical </description></item>
	/// <item><term>UI</term> <description>Error indeterminate</description></item>
	/// </list>
	/// </returns>
FUNCTION TYPE(cExpression AS STRING) AS STRING
	LOCAL uValue AS USUAL
	LOCAL cRet	 AS STRING
	IF String.IsNullOrEmpty(cExpression)	
		cRet := "UE"
	ELSE
		TRY
			uValue := Evaluate(cExpression)
			cRet   := ValType(uValue)
		CATCH  AS Exception
			cRet  := "UE" 
		END TRY
	ENDIF
	RETURN cRet
	
	
/// <summary>
/// Get the type of the class that is used to compile macros
/// </summary>
/// <returns>The type of the currently defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION GetMacroCompiler () AS System.Type
	RETURN XSharp.RuntimeState.MacroCompiler
	
/// <summary>
/// Set the type of the class that must be used to compile macros
/// </summary>
/// <param name="oCompiler">The type of the class that implements the macro compiler. This type MUST implement IMacroCompiler.</param>
/// <returns>The type of the previously defined MacroCompiler. This may be NULL if no type has been set yet and no macros have been compiled.</returns>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
FUNCTION SetMacroCompiler (oCompiler AS System.Type) AS System.Type
VAR old := XSharp.RuntimeState.MacroCompiler
XSharp.RuntimeState.MacroCompiler := oCompiler
RETURN old
