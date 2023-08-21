CLASS MySmtp INHERIT CSMTP
	PROTECT _oOwner    AS OBJECT
	PROTECT _oProgress AS ProgressWindow


CONSTRUCTOR(oOwner, oMail, cServer, nPort) 
	
	SUPER(oMail, cServer, nPort)
	
	_oOwner := oOwner
	
	IF _oOwner != NULL_OBJECT
		IF ! IsMethod(_oOwner, #InternetStatus)
   		_oOwner := NULL_OBJECT
   	ENDIF
	ENDIF

   SELF:TimeOut := 5000

   SELF:MailApplication := "Email Sample - Powered by Visual Objects"

   RETURN SELF


METHOD InternetStatus(nContext, nStatus, xStatus, nStatusLength) 
   LOCAL lRet AS LOGIC

	IF _oOwner != NULL_OBJECT
   	lRet := Send(_oOwner, #InternetStatus, nStatus, xStatus)
	ENDIF

   IF nStatus = INTERNET_STATUS_REQUEST_SENT
      IF _oProgress != NULL_OBJECT
         _oProgress:StepIt(Val(xStatus))
      ENDIF
   ELSEIF nStatus = INTERNET_STATUS_SENDING_REQUEST
      ApplicationExec(ExecWhileEvent)
      IF _oProgress != NULL_OBJECT
         lRet := ! _oProgress:Cancel
      ELSE
         lRet := TRUE
      ENDIF
   ENDIF

   RETURN lRet


ASSIGN Progress(oValue) 
   RETURN _oProgress := oValue


END CLASS
