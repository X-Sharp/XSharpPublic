//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <Summary>Helper class for the BREAK statement</summary>
class XSharp.Internal.WrappedException inherit Exception
	private initonly _value as object
	/// <Summary>The property that is passed by the BREAK statement</summary>
	property value as object get _value

	constructor(value as object)
		_value := value

end class