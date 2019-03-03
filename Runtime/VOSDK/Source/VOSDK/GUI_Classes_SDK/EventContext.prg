CLASS EventContext INHERIT VObject
	PROTECT ptrSelfPtr AS SelfPtr // PTR  dcaton 070301 - type it as it really is!

METHOD Destroy() 

   __WCSelfPtrFree(ptrSelfPtr)

	IF !InCollect()
		UnregisterAxit(SELF)
		ptrSelfPtr := NULL_PTR
	ENDIF

	SUPER:Destroy()

	RETURN SELF

CONSTRUCTOR() 

	SUPER()

	ptrSelfPtr :=__WCSelfPtrAlloc(SELF)

	RETURN 

METHOD @@Override() 
	RETURN NIL

ACCESS SelfPtrStruc 
	RETURN ptrSelfPtr

END CLASS

