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
using XSharp.RDD.ENums

CLASS XSharp.ADS.ADSMemo INHERIT BaseMemo
    PRIVATE oRDD as ADSRDD 

	CONSTRUCTOR(oArea as WorkArea)
		SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD

	// Read & Write		

	/// <inheritdoc />
	VIRTUAL METHOD Flush() 			AS LOGIC
		RETURN oRDD:Flush()

    
	/// <inheritdoc />
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
	/// <inheritdoc />
    VIRTUAL METHOD CloseMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CloseMemFile")

	/// <inheritdoc />
    VIRTUAL METHOD CreateMemFile(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("CreateMemFile")

	/// <inheritdoc />
	VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
	    RETURN oRDD:Unsupported("GetValue")

	/// <inheritdoc />
	VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	    RETURN oRDD:Unsupported("GetValueFile")

	/// <inheritdoc />
    VIRTUAL METHOD OpenMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("OpenMemFile")

	/// <inheritdoc />
	VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        RETURN oRDD:Unsupported("PutValue")

	/// <inheritdoc />
	VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        // Not needed for Advantage. Handled externally
	    RETURN oRDD:Unsupported("PutValueFile")

    #endregion

END CLASS