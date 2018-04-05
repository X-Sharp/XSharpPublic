// Functions_Macro.prg
// Created by    : robert
// Creation Date : 2/16/2017 1:36:32 PM
// Created for   : 
// WorkStation   : ZEUS

using XSharp
/// <summary>
/// Evaluate an expression contained in a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Evaluate(c as string) as __Usual
	/// THROW NotImplementedException{}
return	 __Usual._NIL   


/// <summary>
/// </summary>
/// <param name="ptrEvInf"></param>
/// <returns>
/// </returns>
unsafe function EvalLaunch(ptrEvInf as ptr) as __Usual
	/// THROW NotImplementedException{}
return __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="ptrEvInf"></param>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe function EvalNew(ptrEvInf as ptr,ptrAny as ptr) as logic
	/// THROW NotImplementedException{}
return false   



/// <summary>
/// </summary>
/// <param name="ptrEvInf"></param>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe function EvalPutParam(ptrEvInf as ptr,ptrAny as ptr) as logic
	/// THROW NotImplementedException{}
return false   

/// <summary>
/// </summary>
/// <param name="ptrEvInf"></param>
/// <returns>
/// </returns>
unsafe function EvalRelease(ptrEvInf as ptr) as logic
	/// THROW NotImplementedException{}
return false   





/// <summary>
/// Macro compile a string.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
function MCompile(s as string) as string
	/// THROW NotImplementedException{}
return	 String.Empty   



/// <summary>
/// Evaluate a macro-compiled string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function MExec(c as string) as __Usual
	/// THROW NotImplementedException{}
return	 __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
function MPrepare(s as string) as string
	/// THROW NotImplementedException{}
return	 String.Empty   





/// <summary>
/// Determine the data type of an expression represented as a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function TYPE(c as string) as string

return	 String.Empty   







/// <summary>
/// Return and optionally change the setting that determines whether logical shortcutting is used in macro expression evaluation.
/// </summary>
/// <param name="l"></param>
/// <returns>
/// </returns>
function MCSHORT(l as __Usual) as logic
	/// THROW NotImplementedException{}
	return false  