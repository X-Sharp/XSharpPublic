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
USING XSharp.VFP

INTERNAL CLASS XSharp.VFP.SQLConnection
    PROTECT _oNetConnection AS DbConnection
    PROPERTY DataSource         AS STRING AUTO GET PROTECTED SET
    PROPERTY User               AS STRING AUTO GET PROTECTED SET
    PROPERTY Password           AS STRING AUTO GET PROTECTED SET
    PROPERTY Shared             AS LOGIC AUTO GET PROTECTED SET
    PROPERTY ConnectionString   AS STRING AUTO GET SET

    CONSTRUCTOR(cDataSource AS STRING, cUser AS STRING, cPassword AS STRING, lShared AS LOGIC)
        SELF:DataSource := cDataSource
        SELF:User       := cUser
        SELF:Password   := cPassword
        SELF:Shared     := lShared
        _oNetConnection := SELF:Connect(DataSource, User, Password)
        RETURN 
        

    CONSTRUCTOR(cConnectionString AS STRING, lShared AS LOGIC)
        SELF:Shared     := lShared
        cConnectionString += ";Persist Security Info=TRUE;"
        _oNetConnection     := SELF:Connect(cConnectionString)
        RETURN

     METHOD Connect(cConnStr AS STRING, cUser AS STRING, cPassword AS STRING) AS DbConnection
        LOCAL oConn := NULL AS DbConnection
		TRY
            cConnStr := SQLSupport.Factory:BeforeConnect(cConnStr, cUser, cPassword)
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
		    oBuilder := SQLSupport.Factory:CreateConnectionStringBuilder()
		    oBuilder:ConnectionString := cConnStr
            oConn := SQLSupport.Factory:CreateConnection()
		    cConnStr := oBuilder:ToString()
            TRY
                oConn:ConnectionString := cConnStr
                SELF:ConnectionString     := cConnStr
                oConn:Open()
            CATCH e AS Exception
                cConnStr := SQLSupport.Factory:DriverConnect(NULL, 3, cConnStr)    // 3 = SQL_DRIVER_COMPLETE_REQUIRED
                oConn:ConnectionString    := cConnStr
                SELF:ConnectionString     := cConnStr
                oBuilder:ConnectionString := cConnStr
                oConn:Open()
            END TRY
            IF oConn:State == System.Data.ConnectionState.Open
                SQLSupport.Factory:AfterConnect(oConn)
                IF oBuilder:ContainsKey("Password")
                    SELF:Password   := oBuilder["Password"]
                ELSEIF oBuilder:ContainsKey("pwd")
                    SELF:Password   := oBuilder["pwd"]
                ENDIF
                IF oBuilder:ContainsKey("User Id")
                    SELF:User   := oBuilder["User Id"]
                ELSEIF oBuilder:ContainsKey("uid")
                    SELF:User   := oBuilder["uid"]
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



    METHOD DisConnect AS LOGIC
        IF SELF:Connected
            _oNetConnection:Close()
        ENDIF
        _oNetConnection := NULL            
        RETURN TRUE


END CLASS
