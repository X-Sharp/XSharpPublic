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


CLASS XSharp.Ads.ADSMemo INHERIT BaseMemo
    PRIVATE oRDD as ADSRDD

	CONSTRUCTOR(oArea as WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD

	// Read & Write		

	VIRTUAL METHOD Flush() 			AS LOGIC
		RETURN oRDD:Flush()

    
	VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
        LOCAL aFormat  AS Char[]
        LOCAL wLength  AS WORD
        LOCAL dwLength AS DWORD
        nFldPos += 1
        if oRDD:_Fields[nFldPos ]:fieldType == DbFieldType.Memo
            oRDD:ACECALL(ACE.AdsGetFieldLength(oRDD:m_hTable, (DWord)(nFldPos + 1) , out dwLength))
            return (int) dwLength
        ELSE
            IF oRDD:_Fields[nFldPos]:fieldType == DbFieldType.Date
                aFormat := Char[]{ACE.ADS_MAX_DATEMASK}
                wlength := (Word)aFormat:Length 
                oRDD:ACECALL(ACE.AdsGetDateFormat(aFormat, ref wlength))
                return (int) wLength
            ELSE
                return oRDD:_Fields[nFldPos]:Length
            ENDIF
        ENDIF
        

 


    #region Unsupported
    VIRTUAL METHOD CloseMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CloseMemFile")

    VIRTUAL METHOD CreateMemFile(info AS XSharp.DbOpenInfo) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CreateMemFile")

	VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
	    RETURN oRDD:Unsupported("GetValue")

	VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	    RETURN oRDD:Unsupported("GetValueFile")

    VIRTUAL METHOD OpenMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("OpenMemFile")

	VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        RETURN oRDD:Unsupported("PutValue")

	VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("PutValueFile")

    #endregion

END CLASS