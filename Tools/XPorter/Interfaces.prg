//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


INTERFACE IProgress
	METHOD WriteLine(cText AS STRING) AS VOID     
	METHOD Stop() AS VOID
	METHOD Start() AS VOID
END INTERFACE	
