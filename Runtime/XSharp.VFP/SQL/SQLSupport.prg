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
USING System.Data
USING System.Reflection

INTERNAL CLASS XSharp.VFP.HandleCacheElement
    INTERNAL Number     AS LONG
    INTERNAL @@Value    AS SQLStatement
END CLASS

INTERNAL STATIC CLASS XSharp.VFP.SQLSupport
    STATIC PRIVATE INITONLY Cache AS Dictionary <LONG, HandleCacheElement>
    STATIC INTERNAL UniqueId AS LONG
    STATIC INTERNAL Factory AS ISqlFactory
    STATIC PRIVATE INITONLY aFieldNames AS System.Collections.Generic.Dictionary<STRING, STRING>
    STATIC PRIVATE INITONLY cAllowed AS STRING
    STATIC PRIVATE DefaultValues AS Dictionary<STRING, OBJECT>

    STATIC INTERNAL PROPERTY DispLogin AS LONG GET (LONG) GetDefault(SQLProperty.DispLogin)
    
    STATIC CONSTRUCTOR
        Cache    := Dictionary<LONG, HandleCacheElement>{}
        UniqueId := 0
        Factory  := XSharp.Data.Functions.GetSqlFactory()
		cAllowed   := "ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789_abcdefghijklmnopqrstuvwxyz"	
		aFieldNames := System.Collections.Generic.Dictionary<STRING, STRING>{}
        DefaultValues := Dictionary<STRING, OBJECT>{StringComparer.OrdinalIgnoreCase}
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

    STATIC METHOD GetDefault(sDef AS STRING) AS OBJECT
        IF DefaultValues:ContainsKey(sDef)
            RETURN DefaultValues[sDef]
        ENDIF
        RETURN NULL
        
    STATIC METHOD FindStatement(nId AS LONG) AS OBJECT
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

    STATIC METHOD GetPropertyName(cProperty AS STRING) AS STRING
        LOCAL oType     := Typeof(SQLProperty) AS System.Type
        LOCAL aFields   := oType:GetFields() AS FieldInfo[]
        LOCAL aNames    := List<STRING>{} AS List<STRING>
        FOREACH VAR oFld IN aFields
            IF oFld:IsLiteral .AND. oFld:Name:StartsWith(cProperty, StringComparison.OrdinalIgnoreCase) 
                aNames:Add(oFld:Name)
            ENDIF
        NEXT
        IF aNames:Count == 1
            cProperty := aNames[0]
        ELSE
            cProperty := ""
        ENDIF
        RETURN cProperty

    STATIC METHOD GetSetProperty(nHandle AS LONG, cProperty AS STRING, newValue := NULL AS OBJECT) AS OBJECT
        LOCAL oStmt AS XSharp.VFP.SQLStatement
        LOCAL result := NULL AS OBJECT
        LOCAL cNewProp AS STRING
        cNewProp := GetPropertyName(cProperty)
        IF String.IsNullOrEmpty(cNewProp)
            VAR cMessage := String.Format(__VfpStr(VFPErrors.PROPERTY_UNKNOWN), cProperty)
            THROW Error{cMessage}
        ENDIF
        cProperty := cNewProp
        IF nHandle == 0 // Default values
            IF DefaultValues:ContainsKey(cProperty)
                result := DefaultValues[cProperty]
                IF newValue != NULL
                    DefaultValues[cProperty] := newValue
                    result := 1
                ENDIF
            ENDIF
            RETURN result
        ENDIF
        oStmt := FindStatement(nHandle)
        IF oStmt == NULL_OBJECT
            RETURN -1
        ENDIF
        SWITCH cProperty:ToLower()
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
                // Specifies the time TO wait (IN seconds) before returning a connection time-OUT error. If you specify 0, the wait IS indefinite and a time-OUT error IS never returned. ConnectTimeOut can be 0 TO 600. The DEFAULT IS 15. Read/write.
                result := oStmt:ConnectionTimeOut
                IF newValue IS LONG VAR newVal
                    oStmt:ConnectionTimeOut := newVal
                    result := 1
                ENDIF
           CASE SQLProperty.DataSource
                // The name OF the data source AS defined IN the ODBC.INI file. Read/write.
                result := oStmt:DataSource
                IF newValue IS STRING VAR newVal
                    oStmt:DataSource := newVal
                    result := 1
                ENDIF
 
           CASE SQLProperty.DisconnectRollback
                // Specifies IF a pending transaction IS committed or rolled back when SQLDISCONNECT( ) IS called FOR the last connection handle.
                //The DEFAULT IS FALSE (.F.), indicating that a pending transaction IS committed when SQLDISCONNECT( ) IS called FOR the last connection handle.
                //Specify TRUE (.T.) TO roll back a pending transaction when SQLDISCONNECT( ) IS called FOR the last connection handle.
                //Connections WITH automatic transaction processing are not affected BY THIS setting.
                //
                //Read/write.
               result := oStmt:DisconnectRollback
               IF newValue IS LOGIC VAR newVal
                    oStmt:DisconnectRollback := newVal
                    result := 1
               ENDIF

           CASE SQLProperty.DispLogin // Contains a numeric value that determines when the ODBC Login dialog box IS displayed. DispLogin may assume the following values:

                //1 or DB_PROMPTCOMPLETE (FROM FOXPRO.H).1 IS the default.
                //2 or DB_PROMPTALWAYS (FROM FOXPRO.H).
                //3 or DB_PROMPTNEVER (FROM FOXPRO.H).
                //
                //IF 1 or DB_PROMPTCOMPLETE IS specified, Visual FoxPro displays the ODBC Login dialog box only IF any required information IS missing.
                //IF 2 or DB_PROMPTALWAYS IS specified, the ODBC Login dialog box IS always displayed, making it possible FOR you TO change settings before connecting.
                //IF 3 or DB_PROMPTNEVER IS specified, the ODBC Login dialog box isn't displayed and Visual FoxPro generates an error if the required login information isn't available. Read/write.
                result := SQLSupport.DispLogin
                    
 
           CASE SQLProperty.DispWarnings
                // Specifies IF error messages are displayed (TRUE (.T.)) or are not displayed (FALSE (.F.), the DEFAULT). Read/write.
                result := oStmt:DispWarnings
                IF newValue IS LOGIC VAR newVal
                   oStmt:DispWarnings := newVal
                    result := 1
                ENDIF
            CASE SQLProperty.IdleTimeout
                // The idle timeout interval IN minutes. Active connections are deactivated after the specified time interval. The DEFAULT value IS 0 (wait indefinitely). Read/write.
                result := oStmt:IdleTimeout
                IF newValue IS LONG VAR newVal
                   oStmt:IdleTimeout := newVal
                    result := 1
                ENDIF

           CASE SQLProperty.NativeCommand
                result := oStmt:ODBChstmt:CommandText

           CASE SQLProperty.ODBChdbc
                // The INTERNAL ODBC connection handle, which may be used BY external library files (FLL files) TO CALL ODBC. Read-only.
                result := oStmt:ODBChdbc
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF

           CASE SQLProperty.ODBChstmt
                // The INTERNAL ODBC statement handle, which may be used BY external library files (FLL files) TO CALL ODBC. Read-only.
                result := oStmt:ODBChstmt
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF
           CASE SQLProperty.PacketSize
                // The size OF the network packet used BY the connection. Adjusting THIS value can improve performance. The DEFAULT value IS 4096 bytes (4K). Read/write
 
               result := oStmt:PacketSize
                IF newValue IS LONG VAR newVal
                   oStmt:PacketSize := newVal
                    result := 1
                ENDIF
           CASE SQLProperty.Password
                // The connection password. Read-only.
 
                result := oStmt:Password
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF
                
           CASE SQLProperty.QueryTimeOut
                // Specifies the time TO wait (IN seconds) before returning a general time-OUT error. If you specify 0 (the DEFAULT), the wait IS indefinite and a time-OUT error IS never returned. QueryTimeOut can be 0 TO 600. Read/write.
 
                result := oStmt:QueryTimeOut
                IF newValue IS LONG VAR newVal
                   oStmt:QueryTimeOut := newVal
                    result := 1
                ENDIF
           CASE SQLProperty.Shared
                // Specifies whether the underlying connection IS a shared connection (TRUE (.T.)), or not (FALSE (.F.)). Read-only.
 
                result := oStmt:Shared
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF
                
           CASE SQLProperty.Transactions
                // Contains a numeric value that determines how the connection manages transactions ON the remote table. Transactions may assume the following values:
                //1 or DB_TRANSAUTO (FROM FOXPRO.H).1 IS the default. Transaction processing FOR the remote table IS automatically handled.
                //2 or DB_TRANSMANUAL (FROM FOXPRO.H). Transaction processing IS handled manually through SQLCOMMIT( ) and SQLROLLBACK( ). Read/write.
                result := oStmt:TransactionMode
                IF newValue IS LONG VAR newVal
                    IF newVal == DB_TRANSAUTO .OR. newVal == DB_TRANSMANUAL
                        oStmt:TransactionMode := newVal
                        result := 1
                    ENDIF
                ENDIF
 
           CASE SQLProperty.UserId
                // The user identification. Read-only.
                result := oStmt:UserId
                IF newValue != NULL    // Readonly
                    result := -1
                ENDIF
                
           CASE SQLProperty.WaitTime
                // The amount OF time IN milliseconds that elapses before Visual FoxPro checks IF the SQL statement has completed executing. The DEFAULT IS 100 milliseconds. Read/write.
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
    STATIC METHOD  CleanupColumnName( cColumnName AS  STRING ) AS STRING
		LOCAL sb AS System.Text.StringBuilder
		LOCAL lLastWasOk AS LOGIC
		LOCAL cResult AS STRING
		LOCAL cWork		AS STRING
		IF aFieldNames:ContainsKey(cColumnName)
			RETURN aFieldNames[cColumnName]
		ENDIF		
		// return only allowed characters
		sb  := System.Text.StringBuilder{}
		// When the column is an expresion like CONCAT( foo, bar)
		// Then remove the function name and the last closing param
		// when there is more than one function we remove all functions
		cWork := cColumnName
		DO WHILE cWork:Contains("(") .AND. cWork:Contains(")") .AND. cWork:IndexOf("(") < cWork:IndexOf(")")
			cWork := cWork:Substring(cWork:IndexOf('(')+1)
			cWork := cWork:Substring(0, cWork:LastIndexOf(')'))
		ENDDO
		// Remove the paramter delimiters
		cWork := cWork:Replace(",", "")
		lLastWasOk := FALSE
		FOREACH IMPLIED cChar IN cWork
			IF cAllowed:IndexOf(cChar) >= 0
				sb:Append(cChar)
				lLastWasOk := TRUE
			ELSEIF lLastWasOk
				sb:Append('_')
				lLastWasOk := FALSE
			ENDIF
		NEXT
		IF sb:Length == 0
			// Something like "Count(*)" was passed in
			cWork := cColumnName
			cWork := cWork:Replace("(", "")
			cWork := cWork:Replace(")", "")
			FOREACH IMPLIED cChar IN cWork
				IF cAllowed:IndexOf(cChar) >= 0
					sb:Append(cChar)
					lLastWasOk := TRUE
				ELSEIF lLastWasOk
					sb:Append('_')
					lLastWasOk := FALSE
				ENDIF
			NEXT
		ENDIF	
		IF sb:Length > 1
			DO WHILE sb[sb:Length-1] == c'_'
				sb:Remove(sb:Length-1,1)
			ENDDO
		ENDIF	
		IF sb:Length == 0
			cResult := "EXPR" 
		ELSE	
			cResult := sb:ToString()
		ENDIF
		aFieldNames:Add(cColumnName, cResult)
		RETURN cResult



    
END CLASS

