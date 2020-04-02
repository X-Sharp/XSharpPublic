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
            VAR nFlds   := SELF:FieldCount
            IF lResult
               SELF:ConvertToMemory()
            ENDIF
            FOR VAR nI := 1 TO SELF:_RecCount
                SUPER:GoTo(nI)
                VAR aData := OBJECT[]{nFlds}
                _rows:Add(aData)
                FOR VAR nFld := 1 TO nFlds
                    aData[nFld-1] := SUPER:GetValue(nFld)
                NEXT
            NEXT
            RETURN lResult

        OVERRIDE METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
            VAR lResult := SUPER:Append(lReleaseLock)
            IF lResult
                VAR aData := OBJECT[]{SELF:FieldCount}
                _rows:Add(aData)
            ENDIF
            RETURN lResult

		OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                IF SELF:_RecNo <= _rows:Count .AND. SELF:_RecNo > 0
                    VAR nRow := SELF:_RecNo -1
                    VAR result := _rows[nRow][nFldPos -1]
                    IF result != DBNull.Value
                        RETURN result
                    ELSE
                        RETURN NULL
                    ENDIF
                ENDIF
            ENDIF
            RETURN SUPER:GetValue(nFldPos)

        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            IF nFldPos > 0 .AND. nFldPos <= SELF:FieldCount
                IF SELF:_RecNo <= _rows:Count .AND. SELF:_RecNo > 0
                    VAR nRow := SELF:_RecNo -1
                    _rows[nRow][nFldPos -1] := oValue
                     RETURN TRUE
                ENDIF
            ENDIF
            RETURN SUPER:PutValue(nFldPos, oValue)

        METHOD GetData() AS OBJECT[]
            RETURN SELF:_rows[SELF:_RecNo -1] 

        METHOD Close() AS LOGIC
            LOCAL lOk AS LOGIC
            LOCAL cFileName := SELF:_FileName AS STRING
            LOCAL cMemoName := "" AS STRING
            IF SELF:_Memo IS AbstractMemo VAR memo
                cMemoName := memo:FileName
            ENDIF
            lOk := SUPER:Close()
            IF lOk
                IF File(cFileName)
                    FErase(FPathName())
                ENDIF
                IF ! String.IsNullOrEmpty(cMemoName) .AND. File(cMemoName)
                    FErase(FPathName())
                ENDIF
            ENDIF
            RETURN lOk
    END CLASS
        
  

END NAMESPACE

