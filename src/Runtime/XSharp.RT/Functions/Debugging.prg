//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Diagnostics



/// <include file="XSharp.RT.Docs.xml" path="doc/DebOut/*" />
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


/// <include file="XSharp.RT.Docs.xml" path="doc/CanBreak/*" />
FUNCTION CanBreak() AS LOGIC
	RETURN XSharp.Internal.CompilerServices.CanBreak()


/// <include file="XSharp.RT.Docs.xml" path="doc/Version/*" />
FUNCTION Version() AS STRING
	RETURN "XSharp "+__VERSION__

