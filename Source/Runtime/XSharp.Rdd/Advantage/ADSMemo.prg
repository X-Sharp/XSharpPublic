//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

/// <summary>Advantage Memo support.</summary>
CLASS XSharp.ADS.ADSMemo INHERIT BaseMemo
    PRIVATE oRdd AS ADSRDD

    CONSTRUCTOR(oArea AS Workarea)
        SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD
    #region Helpers
    PROPERTY Table AS IntPtr GET oRdd:_Table

    PRIVATE METHOD _CheckError(nResult AS DWORD) AS LOGIC
        RETURN SELF:oRdd:_CheckError(nResult)
    PRIVATE METHOD Unsupported(strFunctionName AS STRING) AS LOGIC
        SELF:oRdd:Unsupported(strFunctionName )
        RETURN FALSE
    #endregion
    /// <summary>This method is not supported by the AdsMemo class </summary>
    #region Supported
    OVERRIDE METHOD Flush() 			AS LOGIC
        RETURN oRdd:Flush()

        /// <inheritdoc />
    OVERRIDE METHOD GetValueLength(nFldPos AS LONG) AS LONG
        LOCAL fld AS RddFieldInfo
        LOCAL dwField := (DWORD) nFldPos +1 AS DWORD
        fld := SELF:oRdd:_Fields[nFldPos]
        IF fld:FieldType == DbFieldType.Memo
            SELF:_CheckError(ACE.AdsGetFieldLength(SELF:Table, dwField,OUT VAR dwLen))
            RETURN (LONG) dwLen
        ELSEIF fld:FieldType == DbFieldType.Date
            LOCAL chars AS CHAR[]
            chars := CHAR[]{ACE.ADS_MAX_DATEMASK+1}
            LOCAL wLen := (WORD) chars:Length AS WORD
            SELF:_CheckError(ACE.AdsGetDateFormat(chars, REF wLen))
            RETURN wLen
        ENDIF
        RETURN fld:Length
        #endregion
    #region Unsupported
    /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD CloseMemFile( ) AS LOGIC
        // Not needed for Advantage. Handled externally
        RETURN SELF:Unsupported("CloseMemFile")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
        // Not needed for Advantage. Handled externally
        RETURN SELF:Unsupported("CreateMemFile")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
        RETURN SELF:Unsupported("GetValue")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        RETURN SELF:Unsupported("GetValueFile")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD OpenMemFile( info as DbOpenInfo) AS LOGIC
        // Not needed for Advantage. Handled externally
        RETURN SELF:Unsupported("OpenMemFile")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        RETURN SELF:Unsupported("PutValue")

        /// <summary>This method is not supported by the AdsMemo class </summary>
    OVERRIDE METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        // Not needed for Advantage. Handled externally
        RETURN SELF:Unsupported("PutValueFile")

        #endregion

END CLASS
