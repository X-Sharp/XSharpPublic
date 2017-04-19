CLASS CMessage
   //PROTECT cSubBoundary         AS STRING //New 2.7
   PROTECT cCargo              AS STRING
   PROTECT cReplyTo            AS STRING
   PROTECT nPriority           AS INT
   PROTECT cMessageID          AS STRING
   PROTECT cReferences         AS STRING
   
   PROTECT nError              AS DWORD
   
   PROTECT cCharSet             AS STRING    //RvdH 070125
   ~"ONLYEARLY+"
   ~"ONLYEARLY-"
	METHOD __CheckAttachment(cMail, nPart) 
		// syntax error here
		cMail := ...
		
		IF SLen(cMail) == 0
		ENDIF
	RETURN NIL
END CLASS

// 469. Compiler crash with invalid code

