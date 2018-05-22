//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.IO


STATIC CLASS MacroHelpers
	STATIC PRIVATE oMacroCompiler		AS IMacroCompiler
	STATIC PRIVATE oMacroCompilerType	as System.Type
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
	/// <param name="cExpression">The expression to evaluate.</param>
	/// <returns>
/// </returns>
FUNCTION Evaluate(cExpression AS STRING) AS USUAL
	RETURN Evaluate(cExpression, TRUE)
	
	/// <summary>
	/// Evaluate an expression contained in a string.
	/// </summary>
	/// <param name="cExpression">The expression to evaluate.</param>
	/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters.</param>
	/// <returns>
	/// </returns>
FUNCTION Evaluate(cExpression AS STRING, lAllowSingleQuotes AS LOGIC) AS USUAL
	LOCAL oMacro AS XSharp._CODEBLOCK
	LOCAL uRes   AS USUAL
	oMacro := MCompile(cExpression, lAllowSingleQuotes)
	IF oMacro != NULL_OBJECT .and. ! oMacro:IsBlock
		uRes := oMacro:EvalBlock()
	ELSE
		// strange but evaluate on a codeblock returns the block in stead of evaluating it
		uRes := oMacro
	ENDIF
	RETURN uRes
	
	
	
	
	
	/// <summary>
	/// Macro compile a string.
	/// </summary>
	/// <param name="cMacro"></param>
	/// <returns>
	/// </returns>
FUNCTION MCompile(cMacro AS STRING) AS XSharp._CODEBLOCK
	RETURN MCompile(cMacro, TRUE)
	
	
	/// <summary>
	/// Macro compile a string.
	/// </summary>
	/// <param name="cMacro">String to compile</param>
	/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters</param>
	/// <returns>
	/// </returns>
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
	/// Evaluate a macro-compiled string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
FUNCTION MExec(cb AS CODEBLOCK) AS USUAL
	IF cb:PCount() == -1
		RETURN cb:EvalBlock()
	ENDIF
	RETURN cb
	
	/// <summary>
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
FUNCTION MPrepare(s AS STRING) AS STRING
	RETURN s
	
	
	
	
	
	/// <summary>
	/// Determine the data type of an expression represented as a string.
	/// </summary>
	/// <param name="cExpression"></param>
	/// <returns>
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
	
	
FUNCTION GetMacroCompiler () AS System.Type
	RETURN XSharp.RuntimeState.MacroCompiler
	
FUNCTION SetMacroCompiler (oCompiler AS System.Type) AS System.Type
VAR old := XSharp.RuntimeState.MacroCompiler
XSharp.RuntimeState.MacroCompiler := oCompiler
RETURN old
