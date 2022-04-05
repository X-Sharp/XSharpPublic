//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Reflection
USING System.Diagnostics
#pragma options ("az", ON)
/// <summary>
/// Evaluate a code block
/// </summary>
/// <param name="block">Block to evaluate</param>
/// <param name="args">List of arguments</param>
/// <returns>Result of the evaluation</returns>
[DebuggerStepThrough()];
FUNCTION Eval(block AS ICodeblock, args PARAMS USUAL[]) AS USUAL
	LOCAL result AS USUAL
	IF block == NULL
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(block), 1)
	ENDIF
	IF block IS CODEBLOCK VAR cb// compile time codeblock
		result := cb:Eval(args)
	ELSE
		// runtime codeblock ? convert args to object[]
		VAR num := args:Length
		VAR oArgs := OBJECT[]{num}
		FOR VAR i := 0 TO num-1
			oArgs[i] := (OBJECT) args[i]
		NEXT
		result := block:EvalBlock(oArgs)
	ENDIF
	RETURN result

#pragma options ("az", DEFAULT)
/// <summary>
/// Evaluate a code block or an objects Eval() method.
/// </summary>
/// <param name="uCodeBlock">Block to evaluate</param>
/// <param name="args">List of arguments</param>
/// <returns>Result of the evaluation</returns>
FUNCTION Eval( uCodeBlock AS USUAL, args PARAMS USUAL[] ) AS USUAL
	LOCAL result AS USUAL
	IF uCodeBlock:IsNil
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(uCodeBlock), 1)
	ELSEIF uCodeBlock:IsCodeblock
		result := Eval( (CODEBLOCK) uCodeBlock, args )
	ELSE
		result := Eval( (OBJECT) uCodeBlock, args )
	ENDIF
	RETURN result
#pragma options ("az", ON)
/// <summary>
/// Evaluate a code block or an objects Eval() method.
/// </summary>
/// <param name="obj">Object or block to evaluate</param>
/// <param name="args">List of arguments</param>
/// <returns>Result of the evaluation</returns>
FUNCTION Eval( obj AS OBJECT,  args PARAMS USUAL[] ) AS USUAL
	LOCAL result AS USUAL

	IF obj == NULL
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(obj), 1)
	ELSEIF obj IS XSharp.Codeblock
		result := Eval( (CODEBLOCK) obj, args )
    ELSE
        // Search for a method with USUAL[] arguments
		VAR types   := Type[]{ 1 }
		types[0]	:= TYPEOF( USUAL[] )
		VAR oType := obj:GetType()
		LOCAL mi AS MethodInfo
		mi := oType:GetMethod( "Eval", BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase, NULL, types, NULL )

		IF mi != NULL
			VAR pars := OBJECT[]{ 1 }
			pars[0] := args
			result := mi:Invoke( obj , pars )
		ELSE
			THROW Error.ArgumentError( __FUNCTION__, "obj","Argument is not a codeblock"  ,1)
		ENDIF
	ENDIF

	RETURN result
#pragma options ("az", DEFAULT)

/// <summary>
/// Return the number of arguments that a code block is expecting.
/// </summary>
/// <param name="uCodeBlock"></param>
/// <returns>
/// </returns>
FUNCTION CParamCount(oCodeBlock AS CODEBLOCK) AS DWORD
	IF oCodeBlock == NULL_OBJECT
		THROW Error.NullArgumentError(__FUNCTION__, NAMEOF(oCodeBlock), 1)
	ENDIF
	RETURN (DWORD) oCodeBlock:PCount()

/// <exclude/>
FUNCTION __CanEval(uValue AS USUAL) AS LOGIC
	IF uValue:IsCodeblock .AND. uValue != NULL_CODEBLOCK
		RETURN TRUE
	ENDIF
	IF uValue:IsObject .AND. IsMethod(uValue, "Eval")
		RETURN TRUE
	ENDIF
	RETURN FALSE
