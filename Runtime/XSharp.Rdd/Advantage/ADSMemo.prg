//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp
USING XSharp.RDD
using AdvantageClientEngine

CLASS XSharp.RDD.ADSMemo INHERIT BaseMemo
    PRIVATE oRDD as ADSRDD

	CONSTRUCTOR(oArea as WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD

	// Read & Write		

	VIRTUAL METHOD Flush() 			AS LOGIC
		RETURN oRDD:Flush()

	VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
        RETURN oRDD:GetValue(nFldPos)
    
	VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
        //Todo
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	    RETURN oRDD:Unsupported("GetValueFile")

	VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        RETURN oRDD:PutValue(nFldPos, oValue)

	VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("PutValueFile")

	// Memo File Access 
    
    VIRTUAL METHOD CloseMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CloseMemFile")

	VIRTUAL METHOD CreateMemFile(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CreateMemFile")

    VIRTUAL METHOD OpenMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("OpenMemFile")


END CLASS