CLASS MyPop INHERIT CPop
	
	PROTECT _oOwner    AS OBJECT
	PROTECT _oProgress AS ProgressWindow

CONSTRUCTOR(oOwner, cServer, cUid, cPwd, xPort) 
	
	SUPER(cServer, cUid, cPwd, xPort, ogStorage)
	
	_oOwner := oOwner
	
	IF _oOwner != NULL_OBJECT
		IF ! IsMethod(_oOwner, #InternetStatus)
   		_oOwner := NULL_OBJECT
   	ENDIF
	ENDIF
	
	RETURN SELF


METHOD InternetStatus(nContext, nStatus, xStatus, nStatusLength) 

	IF _oOwner != NULL_OBJECT
   	Send(_oOwner, #InternetStatus, nStatus, xStatus)
	ENDIF

   IF _oProgress != Null_Object
      IF nStatus = INTERNET_STATUS_RESPONSE_RECEIVED
         _oProgress:StepIt(Val(xStatus))
      ELSEIF nStatus = INTERNET_STATUS_RECEIVING_RESPONSE
         ApplicationExec(ExecWhileEvent)
         RETURN ! _oProgress:Cancel
      ENDIF
   ENDIF	

   RETURN TRUE


ASSIGN Progress(oValue) 
   RETURN _oProgress := oValue


END CLASS
