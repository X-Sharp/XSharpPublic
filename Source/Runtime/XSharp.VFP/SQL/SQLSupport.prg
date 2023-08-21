//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that the VFP Docs speak about "Statement Handles" for connections and also
// about "statement handles" for statements. These are returned for example by SqlPrepare
// In this code we speak about "connection handles" and "statement handles".



USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.Data
USING System.Data.Common
USING XSharp.VFP
USING XSharp.RDD
USING System.Data

INTERNAL CLASS XSharp.VFP.HandleCacheElement
    INTERNAL Number     AS LONG
    INTERNAL @@Value    AS SQLStatement
END CLASS

INTERNAL STATIC CLASS XSharp.VFP.SQLSupport
    STATIC PRIVATE INITONLY Cache AS Dictionary <LONG, HandleCacheElement>
    STATIC INTERNAL UniqueId AS LONG
    STATIC INTERNAL Factory AS ISqlFactory
    STATIC PRIVATE DefaultValues AS Dictionary<SQLProperty, OBJECT>

    STATIC INTERNAL PROPERTY DispLogin AS LONG GET SQLSupport.GetDefault<LONG>(SQLProperty.DispLogin)

    STATIC CONSTRUCTOR
        Cache    := Dictionary<LONG, HandleCacheElement>{}
        UniqueId := 0
        Factory  := XSharp.Data.Functions.GetSqlFactory()
        DefaultValues := Dictionary<SQLProperty, OBJECT>{}
        DefaultValues.Add(SQLProperty.Asynchronous       ,FALSE)
        DefaultValues.Add(SQLProperty.BatchMode          ,TRUE)
        DefaultValues.Add(SQLProperty.ConnectBusy        ,FALSE)
        DefaultValues.Add(SQLProperty.ConnectString      ,"")
        DefaultValues.Add(SQLProperty.ConnectTimeOut     ,15)
        DefaultValues.Add(SQLProperty.DataSource         ,"")
        DefaultValues.Add(SQLProperty.DisconnectRollback ,FALSE)
        DefaultValues.Add(SQLProperty.DispLogin          ,DB_PROMPTCOMPLETE)
        DefaultValues.Add(SQLProperty.DispWarnings       ,FALSE)
        DefaultValues.Add(SQLProperty.IdleTimeout        ,0)
        //DefaultValues.Add(SQLProperty.ODBChdbc           ,NULL)
        //DefaultValues.Add(SQLProperty.ODBChstmt          ,NULL)
        DefaultValues.Add(SQLProperty.PacketSize         ,4096)
        //DefaultValues.Add(SQLProperty.Password           ,"")
        DefaultValues.Add(SQLProperty.QueryTimeOut       ,0)
        //DefaultValues.Add(SQLProperty.Shared             ,
        DefaultValues.Add(SQLProperty.Transactions       ,DB_TRANSAUTO)
        //DefaultValues.Add(SQLProperty.UserId             ,"")
        DefaultValues.Add(SQLProperty.WaitTime           ,100)

    STATIC METHOD GetDefault<T>(nDef AS SQLProperty) AS T
        IF DefaultValues:ContainsKey(nDef)
            RETURN (T) DefaultValues[nDef]
        ENDIF
        RETURN DEFAULT (T)

    STATIC METHOD FindStatement(nId AS LONG) AS SQLStatement
        IF Cache:ContainsKey(nId)
            VAR element := Cache[nId]
            RETURN element:Value
        ENDIF
        RETURN NULL

    STATIC METHOD GetStatements() AS IList<SQLStatement>
        LOCAL aResult := List<SQLStatement>{} AS IList<SQLStatement>
        FOREACH VAR oElement IN Cache:Values
            aResult:Add(oElement:Value)
        NEXT
        RETURN aResult

    STATIC METHOD AddStatement(oStmt AS SQLStatement) AS LONG
        VAR element := HandleCacheElement{}
        element:Number := ++UniqueId
        element:Value  := oStmt
        Cache:Add(element:Number, element)
        oStmt:Handle := element:Number
        RETURN element:Number


    STATIC METHOD RemoveStatement(nId AS LONG) AS LOGIC
        IF Cache:ContainsKey(nId)
            VAR element := Cache[nId]
            Cache:Remove(nId)
            IF element:Value IS IDisposable VAR disp
                disp:Dispose()
            ENDIF
            RETURN TRUE
        ENDIF
        RETURN FALSE


    STATIC METHOD GetSetProperty(nHandle AS LONG, cProperty AS STRING, newValue := NULL AS OBJECT) AS OBJECT
        LOCAL oStmt AS XSharp.VFP.SQLStatement
        LOCAL result := NULL AS OBJECT
        LOCAL cNewProp AS STRING
        cNewProp := GetPartialEnumName(cProperty, typeof(SQLProperty))
        IF String.IsNullOrEmpty(cNewProp)
            VAR cMessage := __VfpStr(VFPErrors.PROPERTY_UNKNOWN, cProperty)
            THROW Error{cMessage}
        ENDIF
        var nProperty := (SQLProperty) GetSQLProperty(cNewProp)
        IF nHandle == 0 // Default values
            IF DefaultValues:ContainsKey(nProperty)
                result := DefaultValues[nProperty]
                IF newValue != NULL
                    DefaultValues[nProperty] := newValue
                    result := 1
                ENDIF
            ENDIF
            RETURN result
        ENDIF
        oStmt := FindStatement(nHandle)
        IF oStmt == NULL_OBJECT
            RETURN -1
        ENDIF
        SWITCH nProperty
           CASE SQLProperty.Asynchronous
                // Specifies whether result sets are returned synchronously (FALSE (.F.), the DEFAULT), or asynchronously (TRUE (.T.)). Read/write.
               result := oStmt:Asynchronous
               IF newValue IS LOGIC VAR newVal
                    oStmt:Asynchronous := newVal
                    result := 1
               ENDIF

           CASE SQLProperty.BatchMode
                //  Specifies whether SQLEXEC( ) returns result sets all at once (TRUE (.T.), the DEFAULT), or individually WITH SQLMORERESULTS( ) (FALSE (.F.)). Read/write.
                result := oStmt:BatchMode
                IF newValue IS LOGIC VAR newVal
                    oStmt:BatchMode := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.ConnectBusy
                //  Contains TRUE (.T.) IF a shared connection IS busy; OTHERWISE contains FALSE (.F.). Read-only.
                result := oStmt:ConnectBusy
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.ConnectString
                // The login connection string. Read-only.
                result := oStmt:ConnectionString
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.ConnectTimeOut
                result := oStmt:ConnectionTimeOut
                IF newValue IS LONG VAR newVal
                    oStmt:ConnectionTimeOut := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.DataSource
                result := oStmt:DataSource
                IF newValue IS STRING VAR newVal
                    oStmt:DataSource := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.DisconnectRollback
              result := oStmt:DisconnectRollback
               IF newValue IS LOGIC VAR newVal
                    oStmt:DisconnectRollback := newVal
                    result := 1
               ENDIF

           CASE SQLProperty.DispLogin // Contains a numeric value that determines when the ODBC Login dialog box IS displayed. DispLogin may assume the following values:
                result := SQLSupport.DispLogin


           CASE SQLProperty.DispWarnings
                result := oStmt:DispWarnings
                IF newValue IS LOGIC VAR newVal
                   oStmt:DispWarnings := newVal
                    result := 1
                ENDIF
            CASE SQLProperty.IdleTimeout
                result := oStmt:IdleTimeout
                IF newValue IS LONG VAR newVal
                   oStmt:IdleTimeout := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.NativeCommand
                result := oStmt:ODBChstmt:CommandText

           CASE SQLProperty.ODBChdbc
                result := oStmt:ODBChdbc
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.ODBChstmt
                result := oStmt:ODBChstmt
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF
           CASE SQLProperty.PacketSize

               result := oStmt:PacketSize
                IF newValue IS LONG VAR newVal
                   oStmt:PacketSize := newVal
                    result := 1
                ENDIF

          CASE SQLProperty.Password
                result := oStmt:Password
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.QueryTimeOut
                result := oStmt:QueryTimeOut
                IF newValue IS LONG VAR newVal
                   oStmt:QueryTimeOut := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.Shared
                result := oStmt:Shared
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.Transactions
                result := oStmt:TransactionMode
                IF newValue IS LONG VAR newVal
                    IF newVal == DB_TRANSAUTO .OR. newVal == DB_TRANSMANUAL
                        oStmt:TransactionMode := newVal
                        result := 1
                    ENDIF
                ENDIF

           CASE SQLProperty.UserId
                result := oStmt:UserId
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.WaitTime
                result := oStmt:WaitTime
                IF newValue IS LONG VAR newVal
                   oStmt:WaitTime := newVal
                    result := 1
                ENDIF
        OTHERWISE
            result := -1
        END SWITCH
        RETURN result


    // This method is located here so we can keep the  a table with oldname - newname pairs, so we do not have
    // to do this over and over again.




END CLASS

