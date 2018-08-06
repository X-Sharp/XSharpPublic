//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Reflection

/// <summary>
/// Evaluate a code block or an object's Eval() method.
/// </summary>
/// <param name="block"></param>
/// <param name="args"></param>
/// <returns>
/// </returns>
FUNCTION Eval(block AS ICodeblock, args PARAMS USUAL[]) AS USUAL
	LOCAL result AS USUAL
	IF block == null
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(block), 1)
	ENDIF
	IF block IS CODEBLOCK // compile time codeblock
			VAR cb := (CODEBLOCK) block
		result := cb:Eval(args)
	ELSE
		// runtime codeblock ? convert args to object[]
		VAR num := args:Length
		VAR oArgs := OBJECT[]{num}
		FOR VAR i := 1 TO num
			oArgs[i] := (OBJECT) args[i]
		NEXT
		result := block:EvalBlock(oArgs)
	ENDIF
	RETURN result
	
FUNCTION Eval( uCodeBlock AS USUAL, args PARAMS USUAL[] ) AS USUAL
	LOCAL result AS USUAL
	IF uCodeBlock:IsNil
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(uCodeBlock), 1)
	ELSEIF ! uCodeBlock:IsCodeBlock
		result := Eval( (OBJECT) uCodeBlock, args )
	ELSE 
		result := Eval( (CODEBLOCK) uCodeBlock, args )
	ENDIF
	RETURN result
	
FUNCTION Eval( obj AS OBJECT,  args PARAMS USUAL[] ) AS USUAL
	LOCAL result AS USUAL
	
	IF obj == null
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(obj), 1)
	ELSEIF obj IS XSharp.CodeBlock
		result := Eval( (CODEBLOCK) obj, args )
	ELSE
		VAR types   := Type[]{ 1 }
		types[__ARRAYBASE__]	:= TYPEOF( USUAL[] )
		VAR oType := obj:GetType()
		LOCAL mi AS MethodInfo
		mi := oType:GetMethod( "Eval", BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase, null, types, null )
		
		IF mi != null
			VAR pars := OBJECT[]{ 1 }
			pars[__ARRAYBASE__] := args
			result := mi:Invoke( obj , pars )
		ELSE 
			THROW Error.ArgumentError( __ENTITY__, "obj","Argument is not a codeblock"  ,1)
		ENDIF
	ENDIF
	
	RETURN result


/// <summary>
/// Return the number of arguments that a code block is expecting.
/// </summary>
/// <param name="uCodeBlock"></param>
/// <returns>
/// </returns>
FUNCTION CParamCount(oCodeBlock AS CODEBLOCK) AS DWORD
	IF oCodeBlock == null_object
		THROW Error.NullArgumentError(__ENTITY__, NAMEOF(oCodeBlock), 1)
	ENDIF
	RETURN (DWORD) oCodeBlock:PCount()


FUNCTION __CanEval(uValue AS USUAL) AS LOGIC 
	IF uValue:isCodeBlock .and. uValue != NULL_CODEBLOCK
		RETURN TRUE
	ENDIF
	IF uValue:IsObject .and. IsMethod(uValue, "Eval")
		RETURN TRUE
	ENDIF
	RETURN false