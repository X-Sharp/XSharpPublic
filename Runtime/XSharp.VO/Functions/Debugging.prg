using System.Reflection
using System.Diagnostics
/// <summary>
/// Either determine whether the Debugger can be invoked manually or programmatically define a breakpoint in an application.
/// </summary>
/// <param name="nMode">This parameter is ignored in X#</param>
/// <returns>
/// </returns>
/// <Remarks>This function is inlined by the compiler </remarks>
function AltD(nMode as Usual) as void
	if System.Diagnostics.Debugger.IsAttached
		System.Diagnostics.Debugger.Break()
	endif
	return  



/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION _DebOut32( u AS USUAL) AS VOID
   global::Functions._DebOut32( AsString(u))
   return


/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION DebOut32( u AS USUAL) AS VOID
   global::Functions.DebOut32( AsString(u))
   return


/// <summary>
/// Check whether a break occurs within the BEGIN SEQUENCE...END construct.
/// </summary>
/// <returns>
/// </returns>
function CanBreak() as logic
	return XSharp.Internal.CompilerServices.CanBreak()


/// <summary>
/// Return the source file for the active code line
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcFile(dwActivation ) as string
	return global::Functions.ProcFile ( (int) dwActivation +1)

/// <summary>
/// Return the source line number of the last line executed in an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcLine(dwActivation ) as dword
	return global::Functions.ProcLine ( (int) dwActivation +1)

/// <summary>
/// Return the name of an activated entity.
/// </summary>
/// <param name="dwActivation"></param>
/// <returns>
/// </returns>
function ProcName(dwActivation ) as string
return global::Functions.ProcName ( (int) dwActivation +1)


/// <summary>
/// Returns the version of <%APP%> you are using.
/// </summary>
/// <returns>
/// </returns>
function Version() as string
	/// THROW NotImplementedException{}
	return String.Empty   


