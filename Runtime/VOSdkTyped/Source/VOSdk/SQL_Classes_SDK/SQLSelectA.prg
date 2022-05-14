//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//




USING System.Data.Common
USING System.Data
USING System.Collections.Generic



PARTIAL CLASS SQLSelect INHERIT DataServer
    /// <include file="Sql.xml" path="doc/SQLSelect.AppendData/*" />
    PROPERTY AppendData AS ARRAY GET {}


    /// <include file="Sql.xml" path="doc/SQLSelect.AppendFlag/*" />
    PROPERTY AppendFlag AS LOGIC GET SELF:lAppendFlag

    PROPERTY BatchUpdates as LOGIC GET SELF:lBatchUpdates SET lBatchUpdates := value

    /// <include file="Sql.xml" path="doc/SQLSelect.BoF/*" />
    PROPERTY BoF AS LOGIC
        GET
            IF SELF:lAppendFlag
                RETURN FALSE
            ENDIF
            RETURN SELF:lBof
        END GET
    END PROPERTY


    /// <include file="Sql.xml" path="doc/SQLSelect.Connection/*" />
    PROPERTY Connection AS SQLConnection GET SELF:oConn


    /// <include file="Sql.xml" path="doc/SQLSelect.CursorName/*" />
    ACCESS CursorName AS STRING
        RETURN ""


    /// <include file="Sql.xml" path="doc/SQLSelect.CursorName/*" />
    ASSIGN CursorName( cCursorName AS STRING)
        RETURN




    /// <include file="Sql.xml" path="doc/SQLSelect.DBStruct/*" />
    PROPERTY DBStruct AS ARRAY
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
                aStruct[nIndex] :=    { oCol:ColName, oFs:ValType, oFs:Length, oFs:Decimals }
            next


            RETURN aStruct
        END GET
    END PROPERTY




    /// <include file="Sql.xml" path="doc/SQLSelect.Deleted/*" />
    PROPERTY Deleted AS LOGIC GET FALSE


    /// <include file="Sql.xml" path="doc/SQLSelect.EoF/*" />
    PROPERTY EoF AS LOGIC
        GET
            SELF:__ForceOpen()
            IF SELF:lAppendFlag
                RETURN FALSE
            ENDIF
            RETURN SELF:lEof
        END GET
    END PROPERTY


    /// <include file="Sql.xml" path="doc/SQLSelect.ErrInfo/*" />
    PROPERTY ErrInfo AS Error GET SELF:oStmt:ErrInfo


    /// <include file="Sql.xml" path="doc/SQLSelect.FCount/*" />
    PROPERTY FCount AS DWORD
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


    /// <include file="Sql.xml" path="doc/SQLSelect.FOUND/*" />
    PROPERTY FOUND () AS LOGIC GET !SELF:EoF


    /// <include file="Sql.xml" path="doc/SQLSelect.HyperLabel/*" />
    PROPERTY HyperLabel AS HyperLabel
        GET
            SELF:__ForceOpen()
            RETURN SUPER:HyperLabel
        END GET
    END PROPERTY


    /// <include file="Sql.xml" path="doc/SQLSelect.IndexColumns/*" />
    PROPERTY IndexColumns AS List<INT> GET aIndexCol


    /// <include file="Sql.xml" path="doc/SQLSelect.LastRec/*" />
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


    /// <include file="Sql.xml" path="doc/SQLSelect.lRowModified/*" />
    PROPERTY lRowModified AS LOGIC GET oCurrentRow != NULL .AND. oCurrentRow:RowState != DataRowState.Unchanged


    /// <include file="Sql.xml" path="doc/SQLSelect.Modified/*" />
    PROPERTY Modified AS LOGIC GET lRowModified


    /// <include file="Sql.xml" path="doc/SQLSelect.MoreResults/*" />
    PROPERTY MoreResults AS LOGIC GET !SELF:lEof


    /// <include file="Sql.xml" path="doc/SQLSelect.NativeSQL/*" />
    PROPERTY NativeSQL AS STRING GET SELF:oStmt:NativeSQL


    /// <include file="Sql.xml" path="doc/SQLSelect.NullAsBlank/*" />
    PROPERTY NullAsBlank AS LOGIC ;
        GET SELF:lNullAsBlank    ;
        SET SELF:lNullAsBlank := Value


    /// <include file="Sql.xml" path="doc/SQLSelect.NumCols/*" />
    PROPERTY NumCols AS LONG GET nNumCols


    /// <include file="Sql.xml" path="doc/SQLSelect.NumParameters/*" />
    PROPERTY NumParameters AS LONG GET SELF:oStmt:NumParameters


    /// <include file="Sql.xml" path="doc/SQLSelect.NumResultColumns/*" />
    PROPERTY NumResultColumns AS DWORD GET SELF:FCount


    /// <include file="Sql.xml" path="doc/SQLSelect.NumSuccessfulRows/*" />
    PROPERTY NumSuccessfulRows AS LONG GET SELF:nRowCount


    /// <include file="Sql.xml" path="doc/SQLSelect.PrepFlag/*" />
    PROPERTY PrepFlag AS LOGIC GET SELF:oStmt:PrepFlag


    PROPERTY ReadOnly as LOGIC GET SELF:lReadOnly SET SELF:lReadOnly := value

    /// <include file="Sql.xml" path="doc/SQLSelect.RecCount/*" />
    PROPERTY RecCount AS LONG GET SELF:LastRec


    /// <include file="Sql.xml" path="doc/SQLSelect.RecNo/*" />
    PROPERTY RecNo AS LONG
        GET
            IF ! SELF:__ForceOpen()
                RETURN 0
            ENDIF
            RETURN SELF:nCurrentRow+1
        END GET
    END PROPERTY


    /// <include file="Sql.xml" path="doc/SQLSelect.SQLColumns/*" />
    PROPERTY SQLColumns AS SQLColumn[] GET SELF:aSQLColumns


    /// <include file="Sql.xml" path="doc/SQLSelect.SqlData/*" />
    PROPERTY SqlData AS ARRAY GET {}


    /// <include file="Sql.xml" path="doc/SQLSelect.SQLString/*" />
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


    /// <include file="Sql.xml" path="doc/SQLSelect.Statement/*" />
    PROPERTY Statement AS SQLStatement GET SELF:oStmt


    /// <include file="Sql.xml" path="doc/SQLSelect.StatementHandle/*" />
    PROPERTY StatementHandle AS DbCommand GET SELF:oStmt:StatementHandle


    /// <include file="Sql.xml" path="doc/SQLSelect.Status/*" />
    PROPERTY Status AS HyperLabel GET SELF:oStmt:Status


    /// <include file="Sql.xml" path="doc/SQLSelect.TableName/*" />
    PROPERTY TableName AS STRING GET SELF:cTableName


    /// <include file="Sql.xml" path="doc/SQLSelect.TimeStampAsDate/*" />
    PROPERTY TimeStampAsDate AS LOGIC GET SELF:lTimeStampAsDate SET SELF:lTimeStampAsDate := value


    /// <include file="Sql.xml" path="doc/SQLSelect.Used/*" />
    PROPERTY Used AS LOGIC GET SELF:lCsrOpenFlag




END CLASS
