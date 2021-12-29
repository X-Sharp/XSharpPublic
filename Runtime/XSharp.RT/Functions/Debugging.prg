//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Diagnostics


/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <param name="args">List of values to display on the debug terminal</param>

FUNCTION DebOut( args PARAMS USUAL[]) AS VOID
	var  sb := System.Text.StringBuilder{}
	if args != NULL .and. args:Length > 0
		FOREACH var arg in args
			IF sb:Length > 0
				sb:Append(", ")
			END
			sb:Append(AsString(arg))
		NEXT
		DebOut32(sb:ToString())
	endif
	RETURN


/// <summary>
/// Check if a BEGIN SEQUENCE...END construct is active.
/// </summary>
/// <returns>
/// </returns>
FUNCTION CanBreak() AS LOGIC
	RETURN XSharp.Internal.CompilerServices.CanBreak()


/// <summary>
/// Returns the version of X# you are using.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Version() AS STRING
	RETURN "XSharp "+__VERSION__

