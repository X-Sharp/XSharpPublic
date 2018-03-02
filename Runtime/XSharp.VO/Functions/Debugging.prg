
/// <summary>
/// Either determine whether the Debugger can be invoked manually or programmatically define a breakpoint in an application.
/// </summary>
/// <param name="nMode">This parameter is ignored in X#</param>
/// <returns>
/// </returns>
/// <Remarks>This function is inlined by the compiler </remarks>
function AltD(nMode as __Usual) as void
	if System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	endif
	return  


/// <summary>
/// Check whether a break occurs within the BEGIN SEQUENCE...END construct.
/// </summary>
/// <returns>
/// </returns>
function CanBreak() as logic
	/// THROW NotImplementedException{}
	return false   


/// <summary>
/// Return the name of the activated module.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcFile(dwActivation as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcLine(dwActivation as __Usual) as dword
	/// THROW NotImplementedException{}
	return 0   


/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcName(dwActivation as __Usual) as String
	/// THROW NotImplementedException{}
	return String.Empty


/// <summary>
/// Returns the version of <%APP%> you are using.
/// </summary>
/// <returns>
/// </returns>
function Version() as string
	/// THROW NotImplementedException{}
	return String.Empty   


