//
// Copyright (c) B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.IO
USING System.Collections.Generic
USING System.Data
USING System.Diagnostics
#pragma options ("az", ON)
BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFVFPSQL RDD. DBFCDX with support for the FoxPro field types and a List of Object values as backing collection for the data.</summary>
    /// <remarks>At this moment this class assumes that the file is either newly created or opened with 0 rows of data</remarks>
    [DebuggerDisplay("DBFVFPSQL ({Alias,nq})")];
    CLASS DBFVFPSQL INHERIT DBFVFP
        PROTECT _rows   AS List <OBJECT[]>
        #region Overridden properties
        OVERRIDE PROPERTY Driver AS STRING GET "DBFVFPSQL"
        #endregion

        CONSTRUCTOR()
            SUPER()
            _rows := List<OBJECT[]> {}
            RETURN

        OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
            VAR lResult := SUPER:Create(info)
            IF lResult
                SELF:ConvertToMemory()
            ENDIF
            RETURN lResult

        OVERRIDE METHOD Open(info AS DbOpenInfo) AS LOGIC
            VAR lResult := SUPER:Open(info)
            IF lResult
                SELF:ConvertToMemory()
            ENDIF
            RETURN lResult


        OVERRIDE METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
            VAR lResult := SUPER:Append(lReleaseLock)
            IF lResult
                VAR aData := OBJECT[]{SELF:_Fields:Length}
                _rows:Add(aData)
            ENDIF
            RETURN lResult

		OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            IF nFldPos > 0 .AND. nFldPos <= SELF:_Fields:Length
                IF SELF:_RecNo <= _rows:Count .AND. SELF:_RecNo > 0
                    VAR nRow := SELF:_RecNo -1
                    VAR result := _rows[nRow][nFldPos -1]
                    IF result != DBNull.Value
                        RETURN result
                    ENDIF
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF nFldPos > 0 .AND. nFldPos <= SELF:_Fields:Length
                IF SELF:_RecNo <= _rows:Count .AND. SELF:_RecNo > 0
                    VAR nRow := SELF:_RecNo -1
                    _rows[nRow][nFldPos -1] := oValue
                     RETURN TRUE
                ENDIF
            ENDIF
            RETURN SUPER:PutValue(nFldPos, oValue)

        METHOD Close() AS LOGIC
            LOCAL lOk AS LOGIC
            LOCAL cFileName := SELF:_FileName AS STRING
            LOCAL cMemoName := "" AS STRING
            IF SELF:_Memo IS AbstractMemo VAR memo
                cMemoName := memo:FileName
            ENDIF
            lOk := SUPER:Close()
            IF lOk
                FErase(cFileName)
                IF ! String.IsNullOrEmpty(cMemoName)
                    FErase(cMemoName)
                ENDIF
            ENDIF
            RETURN lOk
    END CLASS
        
  

END NAMESPACE

