//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

INTERFACE XSharp.RDD.IMemo
	
	// Read & Write		
	METHOD GetValue(nFldPos AS INT) AS OBJECT
	METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	METHOD GetValueLength(nFldPos AS INT) AS INT
	METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
	METHOD Flush() 			AS LOGIC

	// Memo File Access 
	METHOD CloseMemFile() 	AS LOGIC    
	METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
	METHOD OpenMemFile() 	AS LOGIC   


			
END INTERFACE	
