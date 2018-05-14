//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>Implementation of the IFloat interface that can be used by the RDD system. </summary> 
CLASS XSharp.RDD.DbFLoat IMPLEMENTS IFLoat
	property Value		as Real8 auto
	property Digits		as int auto
	property Decimals	as int auto
	constructor(val as real8, len as int, dec as int)
		Value := val
		Digits := len
		Decimals := dec

end class
