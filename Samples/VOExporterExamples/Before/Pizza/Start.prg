[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XApp
	TRY
		oXApp := XApp{}
		oXApp:Start()
	CATCH e AS Exception
		LOCAL cMessage AS STRING
		cMessage := e:Message
		DO WHILE e:InnerException != NULL_OBJECT
			e := e:InnerException
			cMessage += CRLF+e:Message
		ENDDO
		ErrorBox{NIL, cMessage}:Show()
	END TRY
RETURN 0

CLASS XApp INHERIT App
METHOD Start() 
	local oDlg as OrderDialog
	
  oDlg := OrderDialog{}
  oDlg:Show()
   return nil	


END CLASS
