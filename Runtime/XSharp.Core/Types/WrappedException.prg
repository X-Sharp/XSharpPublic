//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
/// <summary>Helper class for the BREAK statement</summary>
CLASS XSharp.Internal.WrappedException INHERIT Exception
	PRIVATE INITONLY _value AS OBJECT
	/// <summary>The property that is passed by the BREAK statement</summary>
	PROPERTY @@Value AS OBJECT GET _value
	
	/// <summary></summary>	
    CONSTRUCTOR(VALUE AS OBJECT)
		_value := VALUE
	
END CLASS
