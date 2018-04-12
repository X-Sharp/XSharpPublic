// Functions_Codeblock.prg
// Created by    : robert
// Creation Date : 2/16/2017 1:37:24 PM
// Created for   : 
// WorkStation   : ZEUS
using System.Runtime.InteropServices
using System.Reflection

/// <summary>
/// Evaluate a code block or an object's Eval() method.
/// </summary>
/// <param name="block"></param>
/// <param name="args"></param>
/// <returns>
/// </returns>
function Eval(block as ICodeblock, args params usual[]) as usual
	local result as usual
	if block == null
		throw Error.NullArgumentError(__ENTITY__, nameof(block), 1)
	endif
	if block is codeblock // compile time codeblock
			var cb := (codeblock) block
		result := cb:Eval(args)
	else
		// runtime codeblock ? convert args to object[]
		var num := args:Length
		var oArgs := object[]{num}
		for var i := 1 to num
			oArgs[i] := (object) args[i]
		next
		result := block:EvalBlock(oArgs)
	endif
	return result
	
function Eval( uCodeBlock as usual, args params usual[] ) as usual
	local result as usual
	if uCodeBlock:IsNil
		throw Error.NullArgumentError(__ENTITY__, nameof(uCodeBlock), 1)
	elseif ! uCodeBlock:IsCodeBlock
		result := Eval( (object) uCodeBlock, args )
	else 
		result := Eval( (codeblock) uCodeBlock, args )
	endif
	return result
	
function Eval( obj as object,  args params usual[] ) as usual
	local result as usual
	
	if obj == null
		throw Error.NullArgumentError(__ENTITY__, nameof(obj), 1)
	elseif obj is XSharp.CodeBlock
		result := Eval( (codeblock) obj, args )
	else
		var types   := Type[]{ 1 }
		types[1]	:= typeof( usual[] )
		var oType := obj:GetType()
		local mi as MethodInfo
		mi := oType:GetMethod( "Eval", BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase, null, types, null )
		
		if mi != null
			var pars := object[]{ 1 }
			pars[1] := args
			result := mi:Invoke( obj , pars )
		else 
			throw Error.ArgumentError( __ENTITY__, "obj","Argument is not a codeblock"  ,1)
		endif
	endif
	
	return result


/// <summary>
/// Return the number of arguments that a code block is expecting.
/// </summary>
/// <param name="uCodeBlock"></param>
/// <returns>
/// </returns>
function CParamCount(oCodeBlock as CodeBlock) as dword
	if oCodeBlock == null_object
		throw Error.NullArgumentError(__ENTITY__, nameof(oCodeBlock), 1)
	endif
	return (DWORD) oCodeBlock:PCount()