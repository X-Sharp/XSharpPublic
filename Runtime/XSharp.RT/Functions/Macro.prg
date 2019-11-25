//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

		
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/evaluate/*" />
FUNCTION Evaluate(cString AS STRING) AS USUAL
	RETURN Evaluate(cString, TRUE) 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/evaluate/*" />	
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters.</param>
FUNCTION Evaluate(cString AS STRING, lAllowSingleQuotes AS LOGIC) AS USUAL
	LOCAL oMacro AS XSharp._Codeblock 
	LOCAL uRes   AS USUAL
	oMacro := MCompile(cString, lAllowSingleQuotes)
	IF oMacro != NULL_OBJECT .AND. ! oMacro:IsBlock
		uRes := oMacro:EvalBlock()
	ELSE
		// strange but evaluate on a codeblock returns the block in stead of evaluating it
		uRes := oMacro
	ENDIF
	RETURN uRes
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mcompile/*" />	
/// <remarks>MCompile() allows you to use the macro compiler to compile a string and store the compiled results for later execution.  Instead of invoking the macro compiler each time an expression is evaluated, you could speed up your application by compiling an expression only once and executing the compiled form as often as desired.</remarks>
/// <note type="caution">MCompile returns a STRING in VO. It returns a XSharp._Codeblock in .Net.</note>
/// <seealso cref="T:XSharp._Codeblock" />
/// <seealso cref="M:XSharp.RT.Functions.MCompile(System.String,System.Boolean)" />
FUNCTION MCompile(cString AS STRING) AS XSharp._Codeblock
	RETURN MCompile(cString, TRUE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mcompile/*" />	
/// <param name="lAllowSingleQuotes">Should single quotes be allowed as string delimiters</param>
/// <seealso cref="T:XSharp._Codeblock" />
/// <seealso cref="M:XSharp.RT.Functions.MCompile(System.String)" />
FUNCTION MCompile(cString AS STRING, lAllowSingleQuotes AS LOGIC) AS XSharp._Codeblock
	
	VAR oMC := XSharp.RuntimeState.MacroCompiler
	IF oMC != NULL_OBJECT
		VAR oMod := XSharp.RuntimeState.AppModule
		IF oMod == NULL_OBJECT
			XSharp.RuntimeState.AppModule := TYPEOF(XSharp.Core.Functions):Module
		ENDIF
		LOCAL iResult AS ICodeblock
		LOCAL oResult AS XSharp._Codeblock
		LOCAL lIsCodeblock  AS LOGIC
        LOCAL addsMemVars   AS LOGIC
        cString := MPrepare(cString)
		iResult := oMC:Compile(cString, lAllowSingleQuotes, oMod, OUT lIsCodeBlock, OUT addsMemVars)
		oResult := XSharp._Codeblock{iResult, cString, lIsCodeBlock, addsMemVars}
		RETURN oResult
	ENDIF
	RETURN NULL_OBJECT	



INTERNAL FUNCTION MPrepare(cString AS STRING) AS STRING
    IF !cString:Trim():StartsWith("{")
        RETURN cString
    ENDIF
    IF ! NestedMacroHandler.IsNested(cString)
        RETURN cString
    ENDIF
    RETURN NestedMacroHandler.Expand(cString)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mexec/*" />	
/// <note type="caution">MCompile returns a STRING containing PCode tokens in VO.
/// It returns a XSharp._Codeblock in .Net. Therefore the parameter of MExec is a Codeblock</note>
/// <seealso cref="T:XSharp._Codeblock" />
/// <seealso cref="M:XSharp.RT.Functions.MCompile(System.String,System.Boolean)" />
FUNCTION MExec(oBlock AS CODEBLOCK) AS USUAL
	RETURN Eval(oBlock)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/type/*" />	
FUNCTION Type(cString AS STRING) AS STRING
	LOCAL uValue AS USUAL
	LOCAL cRet	 AS STRING
	IF String.IsNullOrEmpty(cString)	
		cRet := "UE"
	ELSE
		TRY
			uValue := Evaluate(cString)
			cRet   := ValType(uValue)
		CATCH  AS Exception
			cRet  := "UE" 
		END TRY
	ENDIF
	RETURN cRet
	
	

