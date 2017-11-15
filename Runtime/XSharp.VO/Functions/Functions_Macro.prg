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
FUNCTION Evaluate(c AS STRING) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   


	/// <summary>
	/// </summary>
	/// <param name="ptrEvInf"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION EvalLaunch(ptrEvInf AS PTR) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// </summary>
	/// <param name="ptrEvInf"></param>
	/// <param name="ptrAny"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION EvalNew(ptrEvInf AS PTR,ptrAny AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   


	
	/// <summary>
	/// </summary>
	/// <param name="ptrEvInf"></param>
	/// <param name="ptrAny"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION EvalPutParam(ptrEvInf AS PTR,ptrAny AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrEvInf"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION EvalRelease(ptrEvInf AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   





/// <summary>
/// Macro compile a string.
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
FUNCTION MCompile(s AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



/// <summary>
/// Evaluate a macro-compiled string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION MExec(c AS STRING) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="s"></param>
/// <returns>
/// </returns>
FUNCTION MPrepare(s AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   





/// <summary>
/// Determine the data type of an expression represented as a string.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION TYPE(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



