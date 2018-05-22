//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Reflection
using System.Diagnostics


/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION _DebOut32( u AS USUAL) AS VOID
   XSharp.Core.Functions._DebOut32( AsString(u))
   return


/// <summary>
/// Write information to the Debug Terminal Program
/// </summary>
/// <returns>
/// </returns>
FUNCTION DebOut32( u AS USUAL) AS VOID
   XSharp.Core.Functions.DebOut32( AsString(u))
   return


/// <summary>
/// Check whether a break occurs within the BEGIN SEQUENCE...END construct.
/// </summary>
/// <returns>
/// </returns>
function CanBreak() as logic
	return XSharp.Internal.CompilerServices.CanBreak()


/// <summary>
/// Returns the version of X# you are using.
/// </summary>
/// <returns>
/// </returns>
function Version() as string
	return "XSharp "+__VERSION__


