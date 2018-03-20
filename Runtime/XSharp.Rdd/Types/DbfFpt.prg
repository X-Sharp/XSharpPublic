//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
CLASS DBFFPT INHERIT DBF 
	CONSTRUCTOR
	SUPER()
	SELF:_oMemo := FptMemo{SELF}
		
VIRTUAL PROPERTY SysName AS STRING GET typeof(DBFFPT):ToString()

    CLASS FptMemo INHERIT BaseMemo  IMPLEMENTS IMemo
	    PROTECT _oRDD AS DBF
	    CONSTRUCTOR (oRDD AS DBF)
		    SUPER(oRDD)
		    SELF:_oRdd := oRDD

    METHOD Flush() 			AS LOGIC		
	    THROW NotImplementedException{}

    METHOD GetValue(nFldPos AS INT) AS OBJECT
	    THROW NotImplementedException{}

    METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	    THROW NotImplementedException{}

    METHOD GetValueLength(nFldPos AS INT) AS INT
	    THROW NotImplementedException{}

    VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	    THROW NotImplementedException{}

    VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	    THROW NotImplementedException{}
    VIRTUAL METHOD CloseMemFile( ) AS LOGIC
	    THROW NotImplementedException{}

    VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
	    THROW NotImplementedException{}

    VIRTUAL METHOD OpenMemFile( ) AS LOGIC
	    THROW NotImplementedException{}
    END CLASS    
END CLASS
END NAMESPACE
