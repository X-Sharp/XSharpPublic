//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Vulcan

begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Exchange the right and left halves of a byte.
	/// </summary>
	/// <param name="b">The byte whose nibbles should be swaped.</param>
	/// <returns>
	/// New value with the nibbles swapped.
	/// </returns>
	FUNCTION SwapByte(b AS BYTE) AS WORD
    return (word)((word)((b & 0x0f) << 4) | ((b >> 4) & 0x0f))

	#endregion
end namespace