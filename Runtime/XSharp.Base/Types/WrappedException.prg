//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <Summary>Helper class for the BREAK statement</summary>
CLASS XSharp.Internal.WrappedException INHERIT Exception
	PRIVATE INITONLY _value AS OBJECT
	/// <Summary>The property that is passed by the BREAK statement</summary>
	PROPERTY value AS OBJECT GET _value
	
	CONSTRUCTOR(value AS OBJECT)
		_value := value
	
END CLASS