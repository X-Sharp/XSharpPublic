//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data.Common
USING System.Collections
PARTIAL CLASS SQLErrorInfo  INHERIT Error
	PROPERTY SQLState           AS STRING AUTO
	PROPERTY NativeError        AS LONG   AUTO
	PROPERTY ErrorFlag          AS LOGIC  AUTO
	PROPERTY ErrorMessage       AS STRING AUTO
	PROPERTY ErrorMessageLen    AS LONG   GET (LONG) SLen(ErrorMessage)
	PROPERTY ReturnCode         AS LONG   AUTO
	
	CONSTRUCTOR( oOriginator, symMethod, Ex ) 
		LOCAL oType AS System.Type
		IF IsNil(Ex)
			Ex := NULL
			SUPER()
		ELSE
			SUPER((Exception) Ex)
			oType := TypeOf(DBException)
			IF oType:IsInstanceOfType(Ex)
				SELF:_SetDbException(Ex)
				SELF:ErrorFlag := TRUE
			ENDIF        
		ENDIF
		
		SELF:SubSystem := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )
		
		IF IsObject( oOriginator )
			SELF:MethodSelf := oOriginator
		ENDIF
		
		IF IsSymbol( symMethod )
			SELF:FuncSym     := symMethod
			SELF:CallFuncSym := symMethod
		ENDIF
		RETURN 
	
	PRIVATE METHOD _SetDbException(oEx AS DbException) AS VOID
		SELF:ErrorMessage := oEx:Message
		SELF:SQLState     := oEx:ErrorCode:ToString("x")
        //		IF TYPEOF(MySql.Data.MySqlClient.MySqlException):isAssignableFrom(oEx:GetType())
        //			LOCAL oMyEx AS MySql.Data.MySqlClient.MySqlException
        //			oMyEx := (MySql.Data.MySqlClient.MySqlException) (OBJECT) oEx
        //			SELF:NativeError := oMyEx:Number
        //			SELF:Source      := oMyEx:Source
        //			
        //		ELSEIF TYPEOF(Oracle.ManagedDataAccess.Client.OracleException):IsAssignableFrom(oEx:GetType())
        //			LOCAL oOraEx AS Oracle.ManagedDataAccess.Client.OracleException
        //			oOraEx := (Oracle.ManagedDataAccess.Client.OracleException) (OBJECT) oEx
        //			SELF:NativeError := oOraEx:Number
        //			FOREACH oError AS Oracle.ManagedDataAccess.Client.OracleError IN oOraEx:Errors
        //				SELF:ErrorMessage += CRLF+oError:Message
        //			NEXT
        //			IF ! STRING.IsNullOrEmpty(oOraEx:@@Procedure)
        //				SELF:ErrorMessage += CRLF+"Procedure: "+oOraEx:@@Procedure
        //			ENDIF
        //			SELF:Source      := oOraEx:Source
        //			
        //		ELSEIF TYPEOF(System.Data.SqlClient.SqlException):IsAssignableFrom(oEx:GetType())
        //			LOCAL oSqlEx AS System.Data.SqlClient.SqlException
        //			oSqlEx := (System.Data.SqlClient.SqlException) (OBJECT) oEx
        //			SELF:NativeError := oSqlEx:Number
        //			SELF:Source      := oSqlEx:Source
        //			FOREACH oError AS System.Data.SqlClient.SqlError IN oSqlEx:Errors
        //				SELF:ErrorMessage += CRLF+oError:Message
        //			NEXT
        //			IF ! STRING.IsNullOrEmpty(oSqlEx:@@Procedure)
        //				SELF:ErrorMessage += CRLF+"Procedure: "+oSqlEx:@@Procedure
        //			ENDIF
        //			
        //		ENDIF
        //SELF
		RETURN 
	
	METHOD ShowErrorMsg() 
//		LOCAL cTitle AS STRING
//		
//		IF ErrorFlag
//			//cTitle :=  AsString( FuncSym )
//			//cTitle +=  "  NativeError: "
//			//cTitle +=  AsString( NativeError )
//			//cTitle +=  "  SQLState: " + SQLState
////			#ifdef VEWA7
////				BusyMessageBox.breakBusyMessage()
////				DevExpress.XtraEditors.XtraMessageBox.Show(ErrorMessage , cTitle ,System.Windows.Forms.MessageBoxButtons.OK)
////				BusyMessageBox.unbreakBusyMessage()
////			#else
////				System.Windows.Forms.MessageBox.Show(ErrorMessage , cTitle ,System.Windows.Forms.MessageBoxButtons.OK)
////			#endif
//			
//		ENDIF
		
		RETURN NIL
	
	
	ACCESS ErrorList
		LOCAL oErr 		AS SQLErrorInfo
		LOCAL oStmt		AS SQLStatement
		LOCAL aRet		AS ARRAY
		
		aRet		:= {} 
		
		IF IsInstanceOf( SELF:MethodSelf , #SqlStatement)
			
			oStmt		:= SELF:MethodSelf 		
			oErr 		:= SELF
			
			WHILE oErr != NULL_OBJECT .and. (Val(oErr:SQLState) <> 0 .or. oErr:NativeError <> 0)
				
				AAdd( aRet, { oErr:NativeError, oErr:SQLState, oErr:ErrorMessage } )
				oErr		:= SQLErrorInfo{ oStmt, SELF:FuncSym}
			ENDDO		
			
		ENDIF
		
		RETURN aRet
	
	
END CLASS

