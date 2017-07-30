//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <Summary>Helper class for the BREAK statement</summary>
CLASS XSharp.Internal.WrappedException INHERIT Exception
	PRIVATE _value as OBJECT
	/// <Summary>The property that is passed by the BREAK statement</summary>
	PROPERTY Value as Object get _value

	CONSTRUCTOR(value as Object)
		_value := value

END CLASS