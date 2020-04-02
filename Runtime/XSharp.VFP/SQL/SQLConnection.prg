//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Data.Common
USING System.Data
USING System.Data.Odbc
USING XSharp.VFP
USING XSharp.Data
USING System.Reflection

INTERNAL CLASS XSharp.VFP.SQLConnection
    PROTECT _oNetConnection AS DbConnection
    PROTECT _oFactory       AS ISqlFactory
    PROTECT _aStatements     AS IList<SQLStatement>

    PROPERTY Factory        AS ISqlFactory GET _oFactory
    PROPERTY NetConnection      AS DbConnection GET _oNetConnection
    PROPERTY State              AS ConnectionState GET _oNetConnection:State
    
    PROPERTY ConnectionTimeOut  AS LONG
        GET
            RETURN _oNetConnection:ConnectionTimeout
        END GET
        SET
            SQLReflection.SetPropertyValue(SELF:NetConnection, "ConnectionTimeout", value)
        END SET
    END PROPERTY       
    PROPERTY ConnectBusy        AS LOGIC  GET _oNetConnection:State == ConnectionState.Executing .OR. _oNetConnection:State == ConnectionState.Fetching

    PROPERTY Shared             AS LOGIC AUTO GET SET
    PRIVATE _DataSource         AS STRING
    PROPERTY DataSource         AS STRING
        GET
            IF SQLReflection.GetPropertyValue(SELF:NetConnection, "DataSource", OUT VAR result)
                RETURN (STRING) result
            ENDIF
            RETURN _DataSource
        END GET
        SET
            _DataSource := Value
            SQLReflection.SetPropertyValue(SELF:NetConnection, "DataSource", value)
        END SET
    END PROPERTY
    PROPERTY UserId             AS STRING AUTO GET SET
    PROPERTY Password           AS STRING AUTO GET SET
    PROPERTY ConnectionString   AS STRING AUTO GET SET
    PROPERTY ODBChdbc   AS DbConnection GET SELF:NetConnection

    METHOD _SetDefaults() AS VOID
        SELF:UserId     := ""
        SELF:Password   := ""
        SELF:DataSource := ""
        SELF:Shared     := FALSE
        SELF:_oFactory  := SQLSupport.Factory
        SELF:_aStatements := List<SQLStatement>{}



    CONSTRUCTOR(cDataSource AS STRING, cUser AS STRING, cPassword AS STRING, lShared AS LOGIC)
        SELF:_SetDefaults()
        SELF:DataSource := cDataSource
        SELF:UserId     := cUser
        SELF:Password   := cPassword
        SELF:Shared     := lShared
        _oNetConnection := SELF:Connect(DataSource, UserId, Password)
        RETURN 
        

    CONSTRUCTOR(cConnectionString AS STRING, lShared AS LOGIC)
        SELF:_SetDefaults()
        SELF:Shared     := lShared
        cConnectionString += ";Persist Security Info=TRUE;"
        _oNetConnection     := SELF:Connect(cConnectionString)
        RETURN

     METHOD Connect(cConnStr AS STRING, cUser AS STRING, cPassword AS STRING) AS DbConnection
        LOCAL oConn := NULL AS DbConnection
		TRY
            cConnStr := SELF:Factory:BeforeConnect(cConnStr, cUser, cPassword)
            oConn := SELF:Connect(cConnStr)
        CATCH e AS Exception
            oConn := NULL
            THROW Error{e}
		END TRY
		RETURN oConn


    METHOD Connect(cConnStr AS STRING) AS DbConnection
        LOCAL oConn := NULL AS DbConnection
		TRY
            LOCAL oBuilder AS DbConnectionStringBuilder
            
		    oBuilder := SELF:Factory:CreateConnectionStringBuilder()
		    oBuilder:ConnectionString := cConnStr
            oConn := SELF:Factory:CreateConnection()
		    cConnStr := oBuilder:ToString()
            TRY
                oConn:ConnectionString := cConnStr
                SELF:ConnectionString     := cConnStr
                oConn:Open()
            CATCH AS Exception
                cConnStr := SELF:Factory:DriverConnect(NULL, SQLSupport.DispLogin, cConnStr)    
                oConn:ConnectionString    := cConnStr
                SELF:ConnectionString     := cConnStr
                oBuilder:ConnectionString := cConnStr
                oConn:Open()
            END TRY
            IF oConn:State == System.Data.ConnectionState.Open
                SELF:Factory:AfterConnect(oConn)
                IF oBuilder:ContainsKey("Password")
                    SELF:Password   := oBuilder["Password"]
                ELSEIF oBuilder:ContainsKey("pwd")
                    SELF:Password   := oBuilder["pwd"]
                ENDIF
                IF oBuilder:ContainsKey("User Id")
                    SELF:UserId   := oBuilder["User Id"]
                ELSEIF oBuilder:ContainsKey("uid")
                    SELF:UserId   := oBuilder["uid"]
                ENDIF
                IF oBuilder:ContainsKey("dsn")
                    SELF:DataSource   := oBuilder["dsn"]
                ENDIF
            ELSE
                oConn := NULL
            ENDIF
        CATCH e AS Exception
            oConn := NULL
            THROW Error{e}
		END TRY
		RETURN oConn

    PROPERTY Connected AS LOGIC
        GET
            IF _oNetConnection != NULL
                RETURN _oNetConnection:State == System.Data.ConnectionState.Open
            ENDIF
            RETURN FALSE
        END GET 
    END PROPERTY

    METHOD Close() AS LOGIC
        IF SELF:Connected
            SELF:_oNetConnection:Close()
            RETURN TRUE
        ENDIF
        RETURN FALSE
        
    METHOD AddStatement(oStmt AS SQLStatement) AS LOGIC
        IF !_aStatements:Contains(oStmt)
            _aStatements:Add(oStmt)
            RETURN TRUE
        ENDIF
        RETURN FALSE

    METHOD RemoveStatement(oStmt AS SQLStatement) AS LOGIC
        IF _aStatements:Contains(oStmt)
            _aStatements:Remove(oStmt)
            IF _aStatements:Count == 0
                SELF:Close()
            ENDIF
            RETURN TRUE
        ENDIF
        RETURN FALSE
        

END CLASS
