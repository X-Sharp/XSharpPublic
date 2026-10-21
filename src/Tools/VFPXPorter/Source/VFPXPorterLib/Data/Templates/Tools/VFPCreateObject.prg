// VFPCreateObject.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING XSharp

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// Static helper that implements VFP <c>CreateObject()</c> with class-name-prefix awareness.<br/>
	/// Generated code calls <see cref="Create"/> with the exporting project's compile-time
	/// <c>VFPX_CLASS_PREFIX</c> constant as the first argument, followed by the original
	/// <c>CreateObject(...)</c> arguments exactly as VFP passes them (class name, then any
	/// optional constructor parameters).<br/>
	/// Tries <c>prefix + className</c> first, so it resolves the actual exported class name when
	/// <c>ClassNamePrefix</c> is set. If that name cannot be resolved to a type, falls back to the
	/// original unprefixed name — so COM ProgIDs (<c>CreateObject("Excel.Application")</c>) and any
	/// other external/.NET type keep working exactly as plain <c>CreateObject()</c> would.
	/// </summary>
	STATIC CLASS __VFPCreateObject

		/// <summary>
		/// Creates an instance of the class named by the second Clipper argument, prefixed with
		/// <paramref name="prefix"/>. Remaining arguments (from the third position on) are passed
		/// through unchanged as constructor parameters.
		/// </summary>
		STATIC METHOD Create( allArgs PARAMS USUAL[] ) AS OBJECT
			// allArgs[1] = prefix, allArgs[2] = className, allArgs[3..] = constructor params
			VAR callArgs := List<Usual>{}
			callArgs:Add( allArgs[1] + (STRING) allArgs[2] )
			FOR LOCAL i := 3 TO allArgs:Length
				callArgs:Add( allArgs[i] )
			NEXT
			TRY
				RETURN (OBJECT) _CallClipFunc( #CreateInstance, callArgs:ToArray() )
			CATCH ex AS Error WHEN ex:Gencode == EG_NOCLASS
				// Prefixed name doesn't resolve to a known type — not one of our exported
				// classes (COM ProgID, external .NET type, etc.). Fall back to the original,
				// unprefixed name exactly as plain CreateObject() would have resolved it.
				callArgs[0] := allArgs[2]
				RETURN (OBJECT) _CallClipFunc( #CreateInstance, callArgs:ToArray() )
			END TRY

	END CLASS

END NAMESPACE // XSharp.VFP.UI

