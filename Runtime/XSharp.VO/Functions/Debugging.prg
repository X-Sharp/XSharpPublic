//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

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
/// Returns the version of <%APP%> you are using.
/// </summary>
/// <returns>
/// </returns>
function Version() as string
	return "XSharp "+__VERSION__


