//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data.Common
USING System.Data
USING System.Collections.Generic

PARTIAL CLASS SqlSelect INHERIT DataServer
   PROPERTY AppendData AS ARRAY GET {}
   
   PROPERTY AppendFlag AS LOGIC GET SELF:lAppendFlag
   
   PROPERTY BoF AS USUAL
      GET
         IF SELF:lAppendFlag
            RETURN FALSE
         ENDIF
         RETURN SELF:lBof
      END GET
   END PROPERTY

   PROPERTY Connection AS SqlConnection GET SELF:oConn
   
   ACCESS CursorName AS STRING
      RETURN ""
      
   ASSIGN CursorName( cCursorName AS STRING)
      RETURN
      
      
   PROPERTY DBStruct AS USUAL
      GET
         LOCAL aStruct       AS ARRAY
         LOCAL nIndex        AS DWORD
         LOCAL oCol			  AS SQLColumn
         LOCAL oFs			  AS FieldSpec
		    //RvdH 050413 Centralize opening of cursor
         IF ! SELF:__ForceOpen()
            RETURN {}
         ENDIF
         
		    // create an array for the dbstructs
         aStruct := ArrayNew( SELF:nNumCols )
         
		    // fill it...
         FOR nIndex := 1 TO SELF:nNumCols
            oCol := SELF:aSQLColumns[nIndex]
            oFs := oCol:FieldSpec
            aStruct[nIndex] :=    ;
            {  oCol:ColName,   ;
            oFs:ValType,    ;
            oFs:Length,     ;
            oFs:Decimals    ;
            }
         NEXT
         
         RETURN aStruct
      END GET
   END PROPERTY

   
   PROPERTY Deleted AS LOGIC GET FALSE
   
   PROPERTY EoF AS USUAL
      GET
         SELF:__ForceOpen()
         IF SELF:lAppendFlag
            RETURN FALSE
         ENDIF
         RETURN SELF:lEof
      END GET
   END PROPERTY
   
   PROPERTY ErrInfo AS Error GET SELF:oStmt:ErrInfo
   
   PROPERTY FCount AS USUAL
      GET
         LOCAL nRet  AS DWORD
         IF ! SELF:__ForceOpen()
            nRet := 0
         ELSE
            nRet := (DWORD) SELF:nNumCols 
         ENDIF
         RETURN nRet
      END GET
   END PROPERTY
   
   PROPERTY FOUND () AS LOGIC GET !SELF:EoF
   
   PROPERTY HyperLabel AS USUAL
      GET
         SELF:__ForceOpen()
         RETURN SUPER:HyperLabel
      END GET
   END PROPERTY
   
   PROPERTY IndexColumns AS List<INT> GET aIndexCol
   
   PROPERTY LastRec AS LONG
      GET
         IF ! SELF:__ForceOpen()
            RETURN 0
         ENDIF
         IF SELF:oTable != NULL
            RETURN SELF:oTable:Rows:Count
         ENDIF
         RETURN 0
      END GET
   END PROPERTY
   
   PROPERTY lRowModified AS LOGIC GET oCurrentRow != NULL .AND. oCurrentRow:RowState != DataRowState.Unchanged

   PROPERTY Modified AS LOGIC GET lRowModified
   
   PROPERTY MoreResults AS LOGIC GET !SELF:lEof
   
   PROPERTY NativeSQL AS STRING GET SELF:oStmt:NativeSQL
   
   PROPERTY NullAsBlank AS LOGIC ;
      GET SELF:lNullAsBlank    ;
      SET SELF:lNullAsBlank := Value
      
      PROPERTY NumCols AS LONG GET nNumCols
      
      PROPERTY NumParameters AS LONG GET SELF:oStmt:NumParameters
      
      PROPERTY NumResultColumns AS DWORD GET SELF:FCount
      
      PROPERTY NumSuccessfulRows AS LONG GET SELF:nRowCount
      
      PROPERTY PrepFlag AS LOGIC GET SELF:oStmt:PrepFlag
      
      PROPERTY RecCount AS USUAL GET SELF:LASTREC
      
      PROPERTY RecNo AS USUAL
         GET
            IF ! SELF:__ForceOpen()
               RETURN 0
            ENDIF
            RETURN SELF:nCurrentRow+1
         END GET
      END PROPERTY
      
      PROPERTY SQLColumns AS SQLColumn[] GET SELF:aSQLColumns
      
      PROPERTY SqlData AS ARRAY GET {}
      
      PROPERTY SQLString AS STRING
      GET
         RETURN SELF:oStmt:SQLString
      END GET
      SET 
         IF SELF:oStmt:PrepFlag
            IF !SELF:Close()
               RETURN 
            ENDIF
         ENDIF
         
         SELF:oStmt:SQLString := value
         
		//  Have a statement?
         IF !String.IsNullOrEmpty(oStmt:SQLString)
            SELF:__FindTableName()
         ENDIF
         RETURN 
      END SET
   END PROPERTY
   
   PROPERTY Statement AS SqlStatement GET SELF:oStmt
   
   PROPERTY StatementHandle AS DbCommand GET SELF:oStmt:StatementHandle
   
   PROPERTY Status AS USUAL GET SELF:oStmt:Status
   
   PROPERTY TableName AS STRING GET SELF:cTableName
   
   PROPERTY TimeStampAsDate AS LOGIC GET SELF:lTimeStampAsDate SET SELF:lTimeStampAsDate := value
   
   PROPERTY Used AS LOGIC GET SELF:lCsrOpenFlag
   
   
END CLASS
