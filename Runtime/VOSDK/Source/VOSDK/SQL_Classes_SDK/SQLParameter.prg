PARTIAL CLASS SqlParameter
	/*
	The SQLParameter class provides a structured way to specify parameters in SQL statements.  In the SQL library, both the cSQLStatement and cSQLSelect Execute methods take an optional array of parameters.  These parameters can be specified as either SQLParameter objects or standard CA-Visual Objects data types.
	
	When a standard CA-Visual Objects data type is detected, a SQLParameter object is automatically created to wrap the value.  Since SQLParameter objects are automatically created, you may never need to use the SQLParameter class directly.
	
	One instance where you are required to use the SQLParameter class is when you wish to specify an OUT parameter.  An OUT parameter is one that returns information from a data source.
	
	EXPORTS:
	
	ALSO SEE:
	
	EXAMPLE:
	Local oStatement as cSQLStatement
	Local oParameter as SQLParameter
	
	// Assume that we already have a valid connection (oConnection)
	
	oStatement:=cSQLStatement{oConnection}
	
	// Create an OUT parameter
	oParameter:=SQLParameter{SQL_PARAM_INPUT_OUTPUT, ,0}
	
	if !oStatement:Execute("Call GetIdentity(?)",{oParameter})
	// Handle Error
	endif
	
	// Get the value returned from the data source
	? oParameter:Value
	
	oStatement:Close()
	oParameter:Destroy()
	
	*/
	
	
	EXPORT IO				AS SHORTINT
	EXPORT UsualType		AS DWORD
	EXPORT CType			AS SHORTINT
	EXPORT ODBCType		AS SHORTINT
	EXPORT Precision		AS DWORD
	EXPORT Scale			AS SHORTINT
	
	EXPORT pData			AS PTR
	EXPORT pBytes			AS PTR
	EXPORT InternalParam	AS LOGIC			// flag that indicates the SQL classes made the parameter (so we can free it)
	
	//DECLARE METHOD Destroy
	DESTRUCTOR() 
	
	SELF:Destroy()
	RETURN 
	
	

METHOD bind(oStatement AS SQLStatement,nPos AS DWORD) AS LONGINT STRICT 
	
	RETURN SQLBindParameter(oStatement:StatementHandle,LoWord(nPos),SELF:IO,SELF:CType,SELF:ODBCType,SELF:Precision,SELF:Scale,SELF:pData,LONGINT(SELF:Precision),SELF:pBytes)
	
	

METHOD Destroy() 
	/*
	This method frees the resources used by the SQLParameter object.
	
	PARAMETERS:
	None.
	
	RETURN VALUES:
	This method always returns TRUE.
	
	REMARKS:
	If you instantiate a SQLParameter object, you should explicitly destroy it when you are finished using it.
	
	*/
	
	IF SELF:pData!=NULL_PTR .AND. !SELF:UsualType==PTR .AND. !SELF:UsualType==PSZ
		MemFree(SELF:pData)
	ENDIF
	
	IF SELF:pBytes!=NULL_PTR
		MemFree(SELF:pBytes)
	ENDIF
	
	//IF !InCollect()
		SELF:pData	:=NULL_PTR
		SELF:pBytes	:=NULL_PTR    
		UnregisterAxit(SELF)
	//ENDIF
	
	RETURN TRUE
	
	
	

CONSTRUCTOR(xValue,nIO,nODBCType,nSize) 
	/*
	Initializes the SQLParameter object.
	
	PARAMETERS:
	nIO - A LongInt that identifies the parameter type.  Must be: SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, or SQL_PARAM_INPUT_OUTPUT.
	
	nODBCType - (optional) A LongInt that identifies the ODBC data type.  Must be a valid ODBC data type or NIL to have the data type automatically selected.
	
	xValue - The initial value of the parameter.
	
	nSize - (optional) The size in bytes of xValue.  This parameter is only used if xValue is a PTR to static memory.
	
	RETURN VALUES:
	Self.
	
	REMARKS:
	The returned object is initialized based on the parameters sent into the Init and is ready to be used as a parameter in the cSQLStatement or cSQLSelect Execute method.
	
	*/
	
	
	Default nIO 			TO SQL_PARAM_INPUT
	Default nODBCType		TO SQL_TYPE_UNKNOWN
	Default nSize			TO 0
	
	SELF:IO			:=nIO
	SELF:ODBCType	:=nODBCType
	
	IF (SELF:pBytes:=MemAlloc(_SIZEOF(LONGINT)))==NULL_PTR
		SQLThrowOutOfMemoryError()
	ENDIF
	
	SELF:SetValue(xValue,nSize)
	
	
	RETURN 
	
	

METHOD SetValue(xValue AS USUAL,nSize AS LONGINT) AS USUAL STRICT 
	/*
	This method assigns a new value to a SQLParameter object.
	
	PARAMETERS:
	xValue - The new value of the parameter.
	
	nSize - The size in bytes of xValue.  This value is only used if xValue is a PTR to static memory.
	
	RETURN VALUES:
	This method always returns xValue.
	
	REMARKS:
	
	
	*/
	
	LOCAL nPos AS DWORD
	
	
	IF !SELF:pData==NULL_PTR .AND. !SELF:UsualType==PTR .AND. !SELF:UsualType==PSZ
		MemFree(SELF:pData)
	ENDIF
	
	SELF:pData		:=NULL_PTR
	SELF:UsualType	:=UsualType(xValue)
	
	IF SELF:UsualType==VOID
		LONGINT(SELF:pBytes):=SQL_NULL_DATA
		
		SELF:Scale 		:=0
		SELF:Precision :=0
		SELF:CType     :=SQL_C_SSHORT
		SELF:ODBCType  :=SQL_SMALLINT
		
		RETURN xValue
		
	ELSEIF SELF:UsualType==STRING
		SELF:CType 		:=SQL_C_CHAR
		SELF:Scale 		:=0
		
		IF SELF:ODBCType==SQL_TIMESTAMP
			xValue		:=PadR(xValue,40,_CHR(0))
			SELF:Scale 	:=6
		ENDIF
		
		SELF:Precision := SLen(xValue)
		
		IF (SELF:pData:=StringAlloc(xValue))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			IF SELF:Precision>255
				SELF:ODBCType:=SQL_LONGVARCHAR
			ELSE
				SELF:ODBCType:=SQL_CHAR
			ENDIF				
		ENDIF
		
	ELSEIF SELF:UsualType==SYMBOL
		xValue		:=Symbol2String(xValue)
		SELF:CType 	:=SQL_C_CHAR
		
		IF (SELF:pData:=StringAlloc(xValue))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		
		SELF:Precision := SLen(xValue)
		SELF:Scale 	:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			IF SELF:Precision>255
				SELF:ODBCType:=SQL_LONGVARCHAR
			ELSE
				SELF:ODBCType:=SQL_CHAR
			ENDIF				
		ENDIF
		
	ELSEIF SELF:UsualType==PSZ
		SELF:CType 	:=SQL_C_CHAR
		SELF:pData 	:=xValue
		SELF:Precision	:= PszLen(xValue)
		SELF:Scale 	:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			IF SELF:Precision>255
				SELF:ODBCType:=SQL_LONGVARCHAR
			ELSE
				SELF:ODBCType:=SQL_CHAR
			ENDIF				
		ENDIF
		
	ELSEIF SELF:UsualType==PTR
		SELF:CType 		:= SQL_C_BINARY
		SELF:pData 		:= xValue
		SELF:Precision := DWORD(nSize)
		SELF:Scale 	:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			SELF:ODBCType:=SQL_LONGVARBINARY
		ENDIF
		
	ELSEIF SELF:UsualType==LONGINT
		SELF:CType 		:=SQL_C_SLONG
		IF (SELF:pData:=MemAlloc(_SIZEOF(LONGINT)))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		LONGINT(SELF:pData)	:=xValue
		SELF:Precision :=_SIZEOF(LONGINT)
		SELF:Scale 		:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			SELF:ODBCType:=SQL_INTEGER
		ENDIF
		
	ELSEIF SELF:UsualType==DATE
		xValue		:=MakeTimeStamp(xValue,0)
		SELF:CType 	:=SQL_C_CHAR
		
		IF (SELF:pData:=StringAlloc(xValue))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		
		SELF:Precision := SLen(xValue)
		SELF:Scale 	:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			SELF:ODBCType:=SQL_TIMESTAMP
		ENDIF
		
	ELSEIF SELF:UsualType==LOGIC
		xValue		:=IIF(xValue,"1","0")
		SELF:CType 	:=SQL_C_CHAR
		
		IF (SELF:pData 	:=StringAlloc(xValue))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		
		SELF:Precision	:=1
		SELF:Scale 		:=0
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			SELF:ODBCType:=SQL_BIT
		ENDIF
		
	ELSEIF SELF:UsualType==FLOAT
		xValue			:=StrTran(NTrim(xValue),",",".")
		SELF:CType 		:=SQL_C_CHAR
		
		IF (SELF:pData:=StringAlloc(xValue))==NULL_PTR
			SQLThrowOutOfMemoryError()
		ENDIF
		
		SELF:Precision :=SLen(xValue)
		
		IF (nPos:=At2(".",xValue))>0
			SELF:Scale 	:= SHORTINT(SELF:Precision-nPos)
		ELSE
			SELF:Scale	:=0
		ENDIF
		
		IF SELF:ODBCType==SQL_TYPE_UNKNOWN
			SELF:ODBCType 	:=SQL_DECIMAL
		ENDIF
	ENDIF
	
	LONGINT(SELF:pBytes):= LONGINT(SELF:Precision)
	RETURN xValue
	
	
	

ACCESS Value AS USUAL STRICT 
	/*
	This access returns the current value of the SQLParameter.
	
	REMARKS:
	The type of the returned value depends on either the value assigned or the value set by the data source.
	
	*/
	
	LOCAL nBytesUsed AS DWORD
	
	
	nBytesUsed:=DWORD(SELF:pBytes)					// get the number of bytes used	
	IF nBytesUsed== DWORD(_CAST,SQL_NULL_DATA)	// is the value null?
		RETURN NIL
	ENDIF
	
	IF SELF:UsualType==STRING
		RETURN Mem2String(SELF:pData,nBytesUsed)
		
	ELSEIF SELF:UsualType==SYMBOL
		RETURN String2Symbol(Mem2String(SELF:pData,nBytesUsed))
		
	ELSEIF SELF:UsualType==LONGINT
		RETURN LONGINT(SELF:pData)
		
	ELSEIF SELF:UsualType==FLOAT
		RETURN Val(Mem2String(SELF:pData,nBytesUsed))
		
	ELSEIF SELF:UsualType==REAL8
		RETURN REAL8(SELF:pData)
		
	ELSEIF SELF:UsualType==LOGIC
		RETURN Mem2String(SELF:pData,1) $ "YT1"
		
	ELSEIF SELF:UsualType==DATE
		RETURN CToDAnsi(Mem2String(SELF:pData,11))
		
	ENDIF
	
	RETURN SELF:pData	
	
END CLASS



#region defines
DEFINE SQL_TYPE_UNKNOWN:=-9999
#endregion
